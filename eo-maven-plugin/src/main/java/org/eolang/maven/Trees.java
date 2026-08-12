/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
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
 * pipeline. This remembers the tree by the SHA-256 of the text that produced
 * it, the way {@link GlobalCache} keys its footprints, so the second walk is
 * skipped.</p>
 *
 * @since 0.74
 */
interface Trees {

    /**
     * The tree of this text, before any pipeline is applied to it.
     * @param text The text of the {@code .eo} source
     * @return The walked XMIR
     * @throws IOException If fails to walk the text
     */
    XML of(String text) throws IOException;

    /**
     * The trees of one build, kept in memory and shared by its goals.
     *
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
        static final Trees INSTANCE = new TsShared();

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
        public XML of(final String text) throws IOException {
            final String hash = new UncheckedText(
                new HexOf(new Sha256DigestOf(new InputOf(text)))
            ).asString();
            if (!this.memo.containsKey(hash)) {
                this.memo.put(
                    hash, new EoSyntax(new InputOf(text), UnaryOperator.<XML>identity()).parsed()
                );
            }
            return this.memo.get(hash);
        }
    }
}
