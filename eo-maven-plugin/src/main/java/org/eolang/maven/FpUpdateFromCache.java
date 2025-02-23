/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.nio.file.Path;
import java.util.function.Supplier;
import org.cactoos.text.TextOf;

/**
 * Footprint that updates target from cache.
 * @since 0.41
 */
final class FpUpdateFromCache extends FpEnvelope {
    /**
     * Ctor.
     * @param cache Lazy path to cache
     */
    FpUpdateFromCache(final Supplier<Path> cache) {
        super(
            (source, target) -> {
                Logger.debug(
                    FpUpdateFromCache.class,
                    "Updating only target %[file]s from cache %[file]s",
                    target, source
                );
                return new Saved(new TextOf(cache.get()), target).value();
            }
        );
    }
}
