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
 * XSD schema with a file, thus making testing much faster.</p>
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
        this.xml = new StrictXML(StrictXmir.reset(src));
    }

    @Override
    public String toString() {
        return this.xml.toString();
    }

    @Override
    public List<String> xpath(final String xpath) {
        return this.xml.xpath(xpath);
    }

    @Override
    public List<XML> nodes(final String xpath) {
        return this.xml.nodes(xpath);
    }

    @Override
    public XML registerNs(final String pfx, final Object uri) {
        return this.xml.registerNs(pfx, uri);
    }

    @Override
    public XML merge(final NamespaceContext ctx) {
        return this.xml.merge(ctx);
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
    public Collection<SAXParseException> validate(final XML xsd) {
        return this.xml.validate(xsd);
    }

    /**
     * Here, we check the location of the XSD in the XML
     * and replace with a new one, if necessary.
     * @param xml Original XML
     * @return New XML with the same node
     */
    private static XML reset(final XML xml) {
        final Node node = xml.inner();
        final List<String> location = xml.xpath("/program/@xsi:noNamespaceSchemaLocation");
        if (!location.isEmpty()) {
            String uri = location.get(0);
            if (uri.startsWith("http")) {
                uri = String.format(
                    "file:///%s",
                    StrictXmir.download(
                        uri,
                        Paths.get("target/xsd").resolve(
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
    private static File download(final String uri, final Path path) {
        final File abs = path.toFile().getAbsoluteFile();
        if (!abs.exists()) {
            if (abs.getParentFile().mkdirs()) {
                Logger.debug(StrictXmir.class, "Directory for %[file]s created", path);
            }
            try {
                Files.write(
                    path,
                    new IoCheckedBytes(
                        new BytesOf(new InputOf(new URI(uri)))
                    ).asBytes()
                );
            } catch (final IOException | URISyntaxException ex) {
                throw new IllegalArgumentException(ex);
            }
        }
        return abs;
    }
}
