/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.UnaryOperator;
import org.cactoos.bytes.Sha256DigestOf;
import org.cactoos.io.InputOf;
import org.cactoos.text.HexOf;
import org.cactoos.text.UncheckedText;
import org.eolang.parser.EoSyntax;

/**
 * The trees walked out of the {@code .eo} sources of this build.
 *
 * <p>Walking a source is the costliest half of making an XMIR and does not
 * depend on the pipeline applied afterwards, yet {@link MjFormat} and
 * {@link Parsing} walk the same text twice. So the first to walk hands the tree
 * over here, keyed by its text, and the other lays its own pipeline over it.</p>
 *
 * @since 0.74
 */
interface Trees {

    /**
     * Remember the tree that was walked out of this text.
     * @param text The text of the {@code .eo} source
     * @param tree The tree walked out of it
     * @throws IOException If fails to keep the tree
     */
    void remember(String text, XML tree) throws IOException;

    /**
     * The tree of this text, with this pipeline applied to it.
     * @param text The text of the {@code .eo} source
     * @param pipeline The XSL pipeline of the goal that asks
     * @return The XMIR
     * @throws IOException If fails to walk the text
     */
    XML tree(String text, UnaryOperator<XML> pipeline) throws IOException;

    /**
     * The trees of one build, waiting in a directory of it.
     *
     * <p>The goals of one build are separate objects that Maven makes one after
     * another, so whatever they share they share through the filesystem, the way
     * {@link GcShared} shares the footprints of earlier builds. A tree waits in
     * {@code 0-walk/hash.xmir} under the target directory, named after the
     * SHA-256 of the text it was walked out of. Only {@code format} writes
     * there, ahead of every goal that reads, and {@code clean} takes it away.</p>
     *
     * <p>Printing a tree indents it, so the file gives back blanks between the
     * elements that the walked tree never had, and a pipeline laid over those
     * yields a different XMIR. XMIR holds no mixed content, so a blank beside an
     * element came from the printer and is dropped on the way in.</p>
     *
     * @since 0.74
     */
    final class TsSaved implements Trees {

        /**
         * Where the trees wait, inside the target directory.
         */
        private static final String DIR = "0-walk";

        /**
         * The target directory of this build.
         */
        private final Path target;

        /**
         * Ctor.
         * @param path The target directory of this build
         */
        TsSaved(final Path path) {
            this.target = path;
        }

        @Override
        public void remember(final String text, final XML tree) throws IOException {
            new Saved(tree.toString(), this.file(text)).value();
        }

        @Override
        public XML tree(final String text, final UnaryOperator<XML> pipeline) throws IOException {
            final Path file = this.file(text);
            final XML walked;
            if (Files.exists(file)) {
                walked = new XMLDocument(file);
                for (final XML blank : walked.nodes("//text()[not(normalize-space())][../*]")) {
                    blank.inner().getParentNode().removeChild(blank.inner());
                }
            } else {
                walked = new EoSyntax(new InputOf(text), UnaryOperator.<XML>identity()).parsed();
            }
            return pipeline.apply(walked);
        }

        /**
         * The file one text waits in.
         * @param text The text of the {@code .eo} source
         * @return The path of it, named after the SHA-256 of the text
         */
        private Path file(final String text) {
            return this.target.resolve(TsSaved.DIR).resolve(
                new UncheckedText(new HexOf(new Sha256DigestOf(new InputOf(text))))
                    .asString().concat(".xmir")
            );
        }
    }
}
