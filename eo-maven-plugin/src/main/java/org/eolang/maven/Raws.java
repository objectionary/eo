/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.file.Path;
import java.util.function.UnaryOperator;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.cactoos.bytes.Sha256DigestOf;
import org.cactoos.io.InputOf;
import org.cactoos.text.HexOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.parser.EoSyntax;

/**
 * The tree the EO parser makes of a source, before any XSL touches it.
 *
 * <p>Two readers of one {@code .eo} file put different XSL trains on top of
 * the grammar, so what they share is the raw tree and nothing else. It is
 * kept here under the hash of the text it was made of, so a rewritten
 * source makes a key of its own and the tree of the old text is never
 * handed out for the new one.</p>
 *
 * @since 0.62.0
 */
final class Raws {

    /**
     * Where the results of earlier builds are looked for and kept.
     */
    private final GlobalCache cache;

    /**
     * The directory the trees are written into.
     */
    private final Path base;

    /**
     * Ctor.
     *
     * @param store Where the results of earlier builds are looked for and kept
     * @param dir The directory the trees are written into
     */
    Raws(final GlobalCache store, final Path dir) {
        this.cache = store;
        this.base = dir;
    }

    /**
     * The raw tree of one source.
     *
     * @param name Identifier of the object in that source
     * @param source The file to read
     * @return The tree the parser makes of it
     * @throws IOException If fails to read or write
     */
    XML of(final String name, final Path source) throws IOException {
        final Path target = new Place(name).make(this.base, MjAssemble.XMIR);
        this.cache.footprint(
            this.base.relativize(target),
            () -> new UncheckedText(
                new HexOf(new Sha256DigestOf(new InputOf(source)))
            ).asString(),
            src -> this.plain(
                new EoSyntax(
                    new InputOf(new TextOf(src).asString()), UnaryOperator.identity()
                ).parsed()
            )
        ).apply(source, target);
        return new XMLDocument(target);
    }

    private String plain(final XML tree) throws IOException {
        try {
            final Transformer transformer =
                TransformerFactory.newInstance().newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "no");
            final StringWriter writer = new StringWriter();
            transformer.transform(new DOMSource(tree.inner()), new StreamResult(writer));
            return writer.toString();
        } catch (final TransformerException ex) {
            throw new IOException("Failed to write down a parsed tree", ex);
        }
    }
}
