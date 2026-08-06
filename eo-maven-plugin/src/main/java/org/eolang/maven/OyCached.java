/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.io.UncheckedIOException;
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
                        throw new UncheckedIOException(exception);
                    }
                }
            );
        } catch (final UncheckedIOException wrap) {
            throw new IOException(
                String.format("Failed to fetch '%s' from the origin objectionary", name), wrap
            );
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
                        throw new UncheckedIOException(exception);
                    }
                }
            );
        } catch (final UncheckedIOException wrap) {
            throw new IOException(
                String.format("Failed to check whether '%s' is a directory", name), wrap
            );
        }
    }

    @Override
    public Iterable<String> children(final String pkg) throws IOException {
        return this.origin.children(pkg);
    }
}
