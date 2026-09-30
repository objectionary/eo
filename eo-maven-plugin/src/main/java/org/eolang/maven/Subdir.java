/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.File;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * A numbered subdirectory of {@code target/eo}.
 *
 * <p>No stage picks its own number any more. The number is the position at
 * which {@code name} is first asked for, among every other name asked for
 * under the same {@code target} in this run: the first stage to ask for a
 * directory this run gets {@code 01-}, the next distinct name gets
 * {@code 02-}, and so on. Two stages that ask for different names therefore
 * never land on the same number, no matter which one runs first, and a
 * stage that asks for the same name twice always lands on the directory it
 * got the first time.</p>
 *
 * @since 0.72.0
 */
final class Subdir {

    /**
     * The number already given to each name asked for so far, per target
     * directory.
     */
    private static final Map<Path, ConcurrentMap<String, Integer>> NUMBERED =
        new ConcurrentHashMap<>();

    /**
     * How many distinct names have been given a number so far, per target
     * directory.
     */
    private static final Map<Path, AtomicInteger> COUNTS = new ConcurrentHashMap<>();

    /**
     * The {@code target/eo} directory this subdirectory lives under.
     */
    private final Path target;

    /**
     * The name of this subdirectory, without its numeric prefix.
     */
    private final String name;

    /**
     * Ctor.
     *
     * @param tgt The {@code target/eo} directory this subdirectory lives under
     * @param nme The name of this subdirectory, without its numeric prefix
     */
    Subdir(final File tgt, final String nme) {
        this(tgt.toPath(), nme);
    }

    /**
     * Ctor.
     *
     * @param tgt The {@code target/eo} directory this subdirectory lives under
     * @param nme The name of this subdirectory, without its numeric prefix
     */
    Subdir(final Path tgt, final String nme) {
        this.target = tgt;
        this.name = nme;
    }

    /**
     * The path of this subdirectory, unless a mojo parameter already
     * names one to use instead.
     *
     * @param configured The value of the parameter, or null when unset
     * @return The path to use
     */
    Path orConfigured(final File configured) {
        final Path path;
        if (configured == null) {
            path = this.path();
        } else {
            path = configured.toPath();
        }
        return path;
    }

    /**
     * The path of this subdirectory.
     *
     * @return The path
     */
    Path path() {
        return this.target.resolve(String.format("%02d-%s", this.number(), this.name));
    }

    private int number() {
        return Subdir.NUMBERED
            .computeIfAbsent(this.target, ignored -> new ConcurrentHashMap<>())
            .computeIfAbsent(this.name, ignored -> Subdir.next(this.target));
    }

    private static int next(final Path target) {
        return Subdir.COUNTS
            .computeIfAbsent(target, ignored -> new AtomicInteger())
            .incrementAndGet();
    }
}
