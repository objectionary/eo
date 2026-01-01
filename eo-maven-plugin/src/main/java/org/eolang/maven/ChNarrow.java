/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

/**
 * Short version of hash.
 *
 * @since 0.28.11
 */
final class ChNarrow implements CommitHash {

    /**
     * Delegate.
     */
    private final CommitHash full;

    /**
     * The main constructor.
     *
     * @param full Delegate
     */
    ChNarrow(final CommitHash full) {
        this.full = full;
    }

    @Override
    public String value() {
        final String hash = this.validHash();
        return hash.substring(0, Math.min(7, hash.length()));
    }

    /**
     * Valid hash.
     *
     * @return Full valid hash.
     */
    private String validHash() {
        final String hash = this.full.value();
        if (hash.isEmpty()) {
            throw new IllegalArgumentException(
                String.format("Hash can't be empty. The delegate %s", this.full)
            );
        }
        return hash;
    }
}
