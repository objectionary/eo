/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

/**
 * A commit hash in its short form ({@link ChNarrow}), read once ({@link ChCached}).
 *
 * @since 0.73.3
 */
final class ChBrief implements CommitHash {

    /**
     * The short hash.
     */
    private final CommitHash origin;

    /**
     * Ctor.
     *
     * @param tag The Git tag in objectionary to take the hash of
     */
    ChBrief(final String tag) {
        this(new ChRemote(tag));
    }

    /**
     * Ctor.
     *
     * @param full The hash to shorten
     */
    ChBrief(final CommitHash full) {
        this.origin = new ChCached(new ChNarrow(full));
    }

    @Override
    public String value() {
        return this.origin.value();
    }
}
