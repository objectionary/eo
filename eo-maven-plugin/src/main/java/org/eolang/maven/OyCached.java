/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.cactoos.Input;

/**
 * Cached Objectionary.
 * @since 0.56.10
 */
final class OyCached implements Objectionary {

    /**
     * The origin.
     */
    private final Objectionary origin;

    /**
     * The cache for programs.
     */
    private final Map<String, Input> programs;

    /**
     * The cache for directories.
     */
    private final Map<String, Boolean> directories;

    /**
     * Ctor.
     * @param oby The objectionary
     */
    OyCached(final Objectionary oby) {
        this(oby, new ConcurrentHashMap<>(0));
    }

    /**
     * Ctor.
     * @param oby The objectionary
     * @param progs The cache for programs
     */
    OyCached(final Objectionary oby, final Map<String, Input> progs) {
        this(oby, progs, new ConcurrentHashMap<>(0));
    }

    /**
     * Ctor.
     * @param oby The objectionary
     * @param progs The cache for programs
     * @param dirs The cache for directories
     */
    OyCached(final Objectionary oby, final Map<String, Input> progs,
        final Map<String, Boolean> dirs) {
        this.origin = oby;
        this.programs = progs;
        this.directories = dirs;
    }

    @Override
    public Input get(final String name) throws IOException {
        try {
            return this.programs.computeIfAbsent(
                name, key -> {
                    try {
                        return this.origin.get(name);
                    } catch (final IOException exception) {
                        throw new OyCached.Uncached(exception);
                    }
                }
            );
        } catch (final OyCached.Uncached wrap) {
            throw wrap.origin();
        }
    }

    @Override
    public boolean contains(final String name) throws IOException {
        return this.programs.containsKey(name)
            || Boolean.TRUE.equals(this.directories.get(name))
            || this.origin.contains(name);
    }

    @Override
    public boolean isDirectory(final String name) throws IOException {
        try {
            return this.directories.computeIfAbsent(
                name, key -> {
                    try {
                        return this.origin.isDirectory(name);
                    } catch (final IOException exception) {
                        throw new OyCached.Uncached(exception);
                    }
                }
            );
        } catch (final OyCached.Uncached wrap) {
            throw wrap.origin();
        }
    }

    @Override
    public Iterable<String> children(final String pkg) throws IOException {
        return this.origin.children(pkg);
    }

    /**
     * Carries an {@link IOException} through a {@code computeIfAbsent()} lambda,
     * which cannot declare checked exceptions.
     * @since 0.74.0
     */
    private static final class Uncached extends RuntimeException {

        /**
         * Serialization identifier.
         */
        private static final long serialVersionUID = 1L;

        /**
         * Ctor.
         * @param cause The original I/O failure
         */
        Uncached(final IOException cause) {
            super(cause);
        }

        /**
         * The original I/O failure.
         * @return The exception
         */
        IOException origin() {
            return (IOException) this.getCause();
        }
    }
}
