/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;

/**
 * The cache of one step, as configured by the user.
 *
 * <p>This is the only place where {@code eo.cacheEnabled} is read, so that no
 * step has to know that the option exists. Kept out of {@link MjSafe} on
 * purpose: its two possible outcomes ({@link GlobalCache} and
 * {@link GcShared}) are exactly the two types that pushed that class over
 * its class-fan-out limit.</p>
 *
 * @since 0.1
 */
final class Caching {

    /**
     * Whether the machine-wide cache is enabled.
     */
    private final boolean enabled;

    /**
     * Directory of the machine-wide cache.
     */
    private final Path cache;

    /**
     * Version of the compiler, folded into the cache key.
     */
    private final String version;

    /**
     * Ctor.
     * @param enabled Whether the machine-wide cache is enabled
     * @param cache Directory of the machine-wide cache
     * @param version Version of the compiler, folded into the cache key
     */
    Caching(final boolean enabled, final Path cache, final String version) {
        this.enabled = enabled;
        this.cache = cache;
        this.version = version;
    }

    /**
     * The cache of one step.
     * @param sub Directory of that step inside the machine-wide cache
     * @return The cache of that step
     */
    GlobalCache store(final String sub) {
        final GlobalCache result;
        if (this.enabled) {
            result = new GcShared(this.cache.resolve(sub), this.version);
        } else {
            result = new GlobalCache.GcFresh();
        }
        return result;
    }
}
