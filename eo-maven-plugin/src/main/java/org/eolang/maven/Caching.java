/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.File;

/**
 * The caching decision configured by the user.
 *
 * <p>This is the only place that turns {@code eo.cache} and
 * {@code eo.cacheEnabled} into a {@link GlobalCache} for one step, so
 * {@link MjSafe} itself no longer has to name {@link GlobalCache},
 * {@link GcShared} and {@link GlobalCache.GcFresh} directly.</p>
 *
 * @since 0.74
 */
final class Caching {

    /**
     * The machine-wide cache directory, as configured by the user.
     */
    private final File dir;

    /**
     * Whether caching is enabled at all.
     */
    private final boolean enabled;

    /**
     * The version that tells one compiler output from another.
     */
    private final String version;

    /**
     * Ctor.
     *
     * @param home The machine-wide cache directory, as configured by the user
     * @param able Whether caching is enabled at all
     * @param ver The version that tells one compiler output from another
     */
    Caching(final File home, final boolean able, final String ver) {
        this.dir = home;
        this.enabled = able;
        this.version = ver;
    }

    /**
     * The cache of one step.
     *
     * @param sub Directory of that step inside the machine-wide cache
     * @return The cache of that step
     */
    GlobalCache forStep(final String sub) {
        final GlobalCache store;
        if (this.enabled) {
            store = new GcShared(this.dir.toPath().resolve(sub), this.version);
        } else {
            store = new GlobalCache.GcFresh();
        }
        return store;
    }
}
