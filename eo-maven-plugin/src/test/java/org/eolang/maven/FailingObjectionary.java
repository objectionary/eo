/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import org.cactoos.Input;
import org.cactoos.iterable.IterableOf;

/**
 * An objectionary whose {@code get()}, {@code contains()} and
 * {@code isDirectory()} always fail with a given {@link IOException}.
 *
 * @since 0.74.0
 */
final class FailingObjectionary implements Objectionary {

    /**
     * The failure to throw.
     */
    private final IOException failure;

    /**
     * Ctor.
     *
     * @param cause The failure to throw
     */
    FailingObjectionary(final IOException cause) {
        this.failure = cause;
    }

    @Override
    public Input get(final String name) throws IOException {
        throw this.failure;
    }

    @Override
    public boolean contains(final String name) throws IOException {
        throw this.failure;
    }

    @Override
    public boolean isDirectory(final String name) throws IOException {
        throw this.failure;
    }

    @Override
    public Iterable<String> children(final String pkg) {
        return new IterableOf<>();
    }
}
