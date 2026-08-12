/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
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
 * {@link Parsing} walk the same text twice, seconds apart, each for its own
 * pipeline. {@link MjFormat} walks first, and hands what it walked over
 * here, keyed by the SHA-256 of the text that produced it, the way
 * {@link GlobalCache} keys its footprints; {@link Parsing} then lays its own
 * pipeline over a copy of that tree instead of walking the text again. Only
 * copies leave this place, so no goal can hand another one the leftovers of
 * its own pipeline.</p>
 *
 * @since 0.74
 */
interface Trees {

    /**
     * Remember the tree that was walked out of this text.
     * @param text The text of the {@code .eo} source
     * @param tree The tree walked out of it
     */
    void remember(String text, XML tree);

    /**
     * The tree of this text, with this pipeline applied to it.
     * @param text The text of the {@code .eo} source
     * @param pipeline The XSL pipeline of the goal that asks
     * @return The XMIR
     * @throws IOException If fails to walk the text
     */
    XML tree(String text, UnaryOperator<XML> pipeline) throws IOException;

    /**
     * The trees of one build, kept in memory and shared by its goals.
     * @since 0.74
     * @todo #6627:30min Bound how much this remembers, since the plugin
     *  realm outlives one module and a settling pass leaves its
     *  intermediate trees behind, so a build of the whole repository may
     *  run out of heap.
     */
    final class TsShared implements Trees {

        /**
         * The one that every goal of one build shares.
         */
        static final Trees INSTANCE = new Trees.TsShared();

        /**
         * Trees, by the SHA-256 of the text that produced them.
         */
        private final Map<String, XML> memo;

        /**
         * Ctor.
         */
        TsShared() {
            this.memo = new ConcurrentHashMap<>(0);
        }

        @Override
        public void remember(final String text, final XML tree) {
            this.memo.put(
                TsShared.hash(text), new XMLDocument(tree.inner().cloneNode(true))
            );
        }

        @Override
        public XML tree(
            final String text, final UnaryOperator<XML> pipeline
        ) throws IOException {
            final String hash = TsShared.hash(text);
            if (!this.memo.containsKey(hash)) {
                this.memo.put(
                    hash, new EoSyntax(new InputOf(text), UnaryOperator.<XML>identity()).parsed()
                );
            }
            return pipeline.apply(
                new XMLDocument(this.memo.get(hash).inner().cloneNode(true))
            );
        }

        /**
         * The key one text is remembered under.
         * @param text The text of the {@code .eo} source
         * @return The SHA-256 of it, in hex
         */
        private static String hash(final String text) {
            return new UncheckedText(
                new HexOf(new Sha256DigestOf(new InputOf(text)))
            ).asString();
        }
    }
}
