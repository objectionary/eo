/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

/**
 * Cached commit hash.
 *
 * @since 0.28.11
 */
final class ChCached implements CommitHash {

    /**
     * Cache.
     */
    private final Unchecked<String> delegate;

    /**
     * Default constructor.
     *
     * @param delegate Delegate
     */
    ChCached(final CommitHash delegate) {
        this.delegate = new Unchecked<>(new Sticky<>(delegate::value));
    }

    @Override
    public String value() {
        return this.delegate.value();
    }
}
