/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Path;
import java.util.function.Supplier;
import org.cactoos.text.TextOf;

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
     * Ctor.
     * @param cache Lazy path to cache
     */
    FpUpdateFromCache(final Supplier<Path> cache) {
        this.cache = cache;
    }

    @Override
    public Path apply(final Path source, final Path target) throws IOException {
        Logger.debug(
            FpUpdateFromCache.class,
            "Updating only target %[file]s from cache %[file]s",
            target, source
        );
        return new Saved(new TextOf(this.cache.get()), target).value();
    }
}
