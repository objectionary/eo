/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.util.function.Supplier;

/**
 * Footprint that updates target from cache.
 * @since 0.41
 */
final class FpUpdateFromCache implements Footprint {

    /**
     * Cached file supplier.
     */
    private final Supplier<Path> cache;

    /**
     * Filesystem abstraction.
     */
    private final Filesystem filesystem;

    /**
     * Ctor.
     * @param cache Lazy path to cache
     */
    FpUpdateFromCache(final Supplier<Path> cache) {
        this(cache, new Filesystem.Real());
    }

    /**
     * Ctor.
     * @param cache Lazy path to cache
     * @param filesystem Filesystem abstraction
     */
    FpUpdateFromCache(final Supplier<Path> cache, final Filesystem filesystem) {
        this.cache = cache;
        this.filesystem = filesystem;
    }

    @Override
    public Path apply(final Path source, final Path target) throws IOException {
        Logger.debug(
            FpUpdateFromCache.class,
            "Updating only target %[file]s from cache %[file]s",
            target, source
        );
        return this.filesystem.save(
            target,
            this.filesystem.read(this.cache.get())
        );
    }
}
