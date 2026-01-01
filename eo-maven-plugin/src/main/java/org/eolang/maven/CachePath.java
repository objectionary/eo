/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.function.Supplier;

/**
 * Function that builds full path to file in global cache.
 * @since 0.41
 */
final class CachePath implements Supplier<Path> {
    /**
     * Cache base directory.
     */
    private final Path base;

    /**
     * Cache version.
     */
    private final String semver;

    /**
     * Git hash.
     */
    private final Supplier<String> hash;

    /**
     * Cache tail path.
     */
    private final Path tail;

    /**
     * Ctor.
     * @param base Base cache directory
     * @param semver Semver as part of absolute cache path
     * @param hash Git hash as part of absolute cache path
     * @param tail The last part of absolute cache path
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    CachePath(
        final Path base, final String semver, final String hash, final Path tail
    ) {
        this(base, semver, () -> hash, tail);
    }

    /**
     * Ctor.
     * @param base Base cache directory
     * @param semver Semver as part of absolute cache path
     * @param hash Git hash as part of absolute cache path
     * @param tail The last part of absolute cache path
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    CachePath(
        final Path base, final String semver, final Supplier<String> hash, final Path tail
    ) {
        this.base = base;
        this.semver = semver;
        this.hash = hash;
        this.tail = tail;
    }

    @Override
    public Path get() {
        return this.base.resolve(this.semver).resolve(this.hash.get()).resolve(this.tail);
    }
}
