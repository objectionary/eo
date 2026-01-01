/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import java.util.function.Supplier;
import org.cactoos.BiFunc;

/**
 * Footprint after application with cache.
 * @since 0.56.7
 */
final class FpAppliedWithCache implements Footprint {

    /**
     * Input.
     */
    private final Footprint input;

    /**
     * Cache.
     */
    private final Supplier<Path> cache;

    /**
     * Rewrite.
     */
    private final BiFunc<Path, Path, Boolean> rewrite;

    /**
     * Cache enabled?
     */
    private final boolean caching;

    /**
     * Ctor.
     * @param ipt Input
     * @param che Cache
     * @param rwrte Rewrite
     * @param enabled Cache enabled?
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    FpAppliedWithCache(
        final Footprint ipt,
        final Supplier<Path> che,
        final BiFunc<Path, Path, Boolean> rwrte,
        final boolean enabled
    ) {
        this.input = ipt;
        this.cache = che;
        this.rewrite = rwrte;
        this.caching = enabled;
    }

    @Override
    public Path apply(final Path source, final Path target) throws IOException {
        final Footprint both = new FpUpdateBoth(this.input, this.cache);
        return new FpFork(
            this.rewrite,
            new FpFork(this.caching, both, this.input),
            new FpIfTargetExists(
                new FpIgnore(),
                new FpFork(
                    this.caching,
                    new FpIfTargetExists(
                        trgt -> this.cache.get(),
                        new FpUpdateFromCache(this.cache),
                        both
                    ),
                    this.input
                )
            )
        ).apply(source, target);
    }
}
