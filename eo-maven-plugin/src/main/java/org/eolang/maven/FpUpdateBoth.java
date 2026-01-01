/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.nio.file.Path;
import java.util.function.Supplier;
import org.cactoos.text.TextOf;

/**
 * Footprint that updates target from content function and updates cache from target.
 * @since 0.41
 */
final class FpUpdateBoth extends FpEnvelope {
    /**
     * Ctor.
     * @param origin Original footprint that updates target from source
     * @param cache Lazy path to cache
     */
    FpUpdateBoth(final Footprint origin, final Supplier<Path> cache) {
        super(
            (source, target) -> {
                final Path che = cache.get();
                Logger.debug(
                    FpUpdateBoth.class,
                    "Updating target %[file]s and cache %[file]s from source %[file]s",
                    target, che, source
                );
                origin.apply(source, target);
                new Saved(new TextOf(target), che).value();
                return target;
            }
        );
    }
}
