/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.parser;

import com.jcabi.log.Logger;
import com.jcabi.xml.StrictXML;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import javax.xml.namespace.NamespaceContext;
import org.cactoos.bytes.BytesOf;
import org.cactoos.bytes.IoCheckedBytes;
import org.cactoos.io.InputOf;
import org.w3c.dom.Node;
import org.xembly.Directives;
import org.xembly.Xembler;
import org.xml.sax.SAXParseException;

/**
 * XMIR that validates itself right after construction.
 *
 * <p>This class is supposed to be used ONLY for testing, because
 * it modifies the XML encapsulated: it replaces the location of
 * XSD schema with a file, thus making testing much faster. However,
 * the original location of the XSD schema will be lost/removed
 * from the XML. Thus, you better save the XML to disc and only
 * then encapsulate it into this decorator.</p>
 *
 * @since 0.49.0
 */
@SuppressWarnings("PMD.TooManyMethods")
public final class StrictXmir implements XML {

    /**
     * The XML.
     */
    private final XML xml;

    /**
     * Ctor.
     * @param src The source
     */
    public StrictXmir(final XML src) {
        this(src, Paths.get("target/xsd"));
    }

    /**
     * Ctor.
     * @param src The source
     * @param tmp The directory with cached XSD files
     */
    public StrictXmir(final XML src, final Path tmp) {
        this.xml = new StrictXML(StrictXmir.reset(src, tmp));
    }

    @Override
    public String toString() {
        return this.xml.toString();
    }

    @Override
    public List<String> xpath(final String query) {
        return this.xml.xpath(query);
    }

    @Override
    public List<XML> nodes(final String query) {
        return this.xml.nodes(query);
    }

    @Override
    public XML registerNs(final String prefix, final Object uri) {
        return this.xml.registerNs(prefix, uri);
    }

    @Override
    public XML merge(final NamespaceContext context) {
        return this.xml.merge(context);
    }

    @Override
    @Deprecated
    public Node node() {
        throw new UnsupportedOperationException("deprecated");
    }

    @Override
    public Node inner() {
        return this.xml.inner();
    }

    @Override
    public Node deepCopy() {
        return this.xml.deepCopy();
    }

    @Override
    public Collection<SAXParseException> validate() {
        return this.xml.validate();
    }

    @Override
    public Collection<SAXParseException> validate(final XML schema) {
        return this.xml.validate(schema);
    }

    /**
     * Here, we check the location of the XSD in the XML
     * and replace with a new one, if necessary.
     * @param xml Original XML
     * @param tmp Directory with cached XSD files
     * @return New XML with the same node
     */
    private static XML reset(final XML xml, final Path tmp) {
        final Node node = xml.inner();
        final List<String> location = xml.xpath("/program/@xsi:noNamespaceSchemaLocation");
        if (!location.isEmpty()) {
            String uri = location.get(0);
            if (uri.startsWith("http")) {
                uri = String.format(
                    "file:///%s",
                    StrictXmir.download(
                        uri,
                        tmp.resolve(
                            uri.substring(uri.lastIndexOf('/') + 1)
                        )
                    ).toString().replace("\\", "/")
                );
            }
            new Xembler(
                new Directives().xpath("/program").attr(
                    "noNamespaceSchemaLocation xsi http://www.w3.org/2001/XMLSchema-instance",
                    uri
                )
            ).applyQuietly(node);
        }
        return new XMLDocument(node);
    }

    /**
     * Download URI from Internet and save to file.
     * @param uri The URI
     * @param path The file
     * @return Where it was saved
     */
    @SuppressWarnings("PMD.CognitiveComplexity")
    private static File download(final String uri, final Path path) {
        final File abs = path.toFile().getAbsoluteFile();
        if (!abs.exists()) {
            if (abs.getParentFile().mkdirs()) {
                Logger.debug(StrictXmir.class, "Directory for %[file]s created", path);
            }
            int attempt = 0;
            while (true) {
                ++attempt;
                try {
                    Files.write(
                        path,
                        new IoCheckedBytes(
                            new BytesOf(new InputOf(new URI(uri)))
                        ).asBytes()
                    );
                    break;
                } catch (final IOException ex) {
                    if (attempt < 3) {
                        Logger.warn(
                            StrictXmir.class,
                            "Attempt #%d failed to download %s to %s: %[exception]s",
                            attempt,
                            uri,
                            path,
                            ex
                        );
                        continue;
                    }
                    throw new IllegalArgumentException(
                        String.format("Failed to download %s to %s", uri, path),
                        ex
                    );
                } catch (final URISyntaxException ex) {
                    throw new IllegalArgumentException(
                        String.format("Wrong URI: %s", uri),
                        ex
                    );
                }
            }
        }
        return abs;
    }
}
