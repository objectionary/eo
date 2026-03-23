/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.cactoos.Scalar;

/**
 * Hash of tag.
 *
 * @since 0.28.11
 */
@FunctionalInterface
interface CommitHash extends Scalar<String> {
    /**
     * Fake commit hash for testing.
     */
    CommitHash FAKE = new CommitHash.ChConstant("abcdef");

    @Override
    String value();

    /**
     * Hardcoded commit hash.
     *
     * @since 0.28.11
     */
    final class ChConstant implements CommitHash {

        /**
         * Hardcoded value.
         */
        private final String hash;

        /**
         * The main constructor.
         *
         * @param hash Hardcoded value.
         */
        ChConstant(final String hash) {
            this.hash = hash;
        }

        @Override
        public String value() {
            return this.hash;
        }
    }
}
