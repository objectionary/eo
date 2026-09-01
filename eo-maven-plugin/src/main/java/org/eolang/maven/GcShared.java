/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Supplier;
import org.cactoos.BiFunc;
import org.cactoos.Func;

/**
 * The cache that lives on this machine and is reused by later builds.
 *
 * <p>A file is kept under {@code dir/key/hash/tail}, where the key tells the
 * output of one version of the compiler apart from another.</p>
 *
 * @since 0.74
 */
final class GcShared implements GlobalCache {

    /**
     * Directory of this step inside the machine-wide cache.
     */
    private final Path dir;

    /**
     * The segment that tells one compiler output from another.
     */
    private final String key;

    /**
     * Guard, shared by every file of one run, see {@link ConcurrentCache}.
     */
    private final ConcurrentCache guard;

    /**
     * Ctor.
     * @param dir Directory of this step inside the machine-wide cache
     * @param key The segment that tells one compiler output from another
     */
    GcShared(final Path dir, final String key) {
        this(dir, key, new ConcurrentCache());
    }

    /**
     * Ctor.
     * @param dir Directory of this step inside the machine-wide cache
     * @param key The segment that tells one compiler output from another
     * @param guard Guard shared with the cache this one was derived from
     */
    private GcShared(final Path dir, final String key, final ConcurrentCache guard) {
        this.dir = dir;
        this.key = key;
        this.guard = guard;
    }

    @Override
    public Footprint footprint(
        final Path tail, final Supplier<String> hash, final Func<Path, String> compile
    ) {
        return (source, target) -> {
            this.guard.apply(
                source, target, tail,
                new Cache(new CachePath(this.dir, this.key, hash, Paths.get(".")), compile)
            );
            return target;
        };
    }

    @Override
    public Footprint kept(
        final Path tail,
        final Supplier<String> hash,
        final BiFunc<Path, Path, Boolean> rewrite,
        final Footprint made
    ) {
        return new FpAppliedWithCache(
            made, new CachePath(this.dir, this.key, hash, tail), rewrite, true
        );
    }

    @Override
    public GlobalCache with(final String segment) {
        return new GcShared(
            this.dir, String.format("%s-%s", this.key, segment), this.guard
        );
    }
}
