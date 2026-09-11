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
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.eolang.parser.EoSyntax;

/**
 * The tree the EO parser makes of a source, before any XSL touches it.
 *
 * <p>Two goals of one build read the same {@code .eo} file: {@code format}
 * parses it to print it back and compare, and {@code parse} parses it again
 * seconds later to make its XMIR, so a clean build of {@code eo-runtime}
 * runs the EO grammar over its sources twice. The grammar is by far the
 * costliest part of either goal, while the XSL trains they put on top of it
 * differ, so what the two share is the raw tree and nothing else.</p>
 *
 * <p>That tree is kept here, under the hash of the text it was made of, so
 * the second reader of a file finds it instead of parsing it again. The key
 * is the content and not the name, which is what makes it safe for the
 * {@code format} goal to rewrite a source it has just parsed: the rewritten
 * text is a different key, and the tree of the old text is never handed out
 * for the new one.</p>
 *
 * @since 0.62.0
 */
final class Raws {

    /**
     * Subdirectory for the raw trees.
     */
    static final String CACHE = "raws";

    /**
     * The directory of this build the trees are written into.
     */
    static final String DIR = "0-raw";

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
        final Sha sha = new Sha(source);
        this.cache.footprint(
            this.base.relativize(target),
            sha::toString,
            src -> Raws.plain(
                new EoSyntax(
                    new InputOf(new TextOf(src).asString()), UnaryOperator.identity()
                ).parsed()
            )
        ).apply(source, target);
        return new XMLDocument(target);
    }

    /**
     * The tree as text, with no whitespace of its own.
     *
     * <p>An indented tree is not the same tree: the newlines a pretty
     * printer puts between elements come back as text nodes of their own,
     * and the XSL train the reader puts on top of the tree sees children
     * that the parser never made. So the text kept here carries the tree
     * and nothing else.</p>
     *
     * @param tree The tree to write down
     * @return The text of it
     * @throws IOException If fails to write
     */
    private static String plain(final XML tree) throws IOException {
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
