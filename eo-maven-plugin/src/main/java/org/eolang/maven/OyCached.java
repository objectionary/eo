/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.cactoos.Input;

/**
 * Cached Objectionary.
 * @since 0.56.10
 */
final class OyCached implements Objectionary {

    /**
     * The origin.
     */
    private final Objectionary origin;

    /**
     * The cache.
     */
    private final Map<String, Input> cache;

    /**
     * Ctor.
     * @param oby The objectionary
     */
    OyCached(final Objectionary oby) {
        this(oby, new ConcurrentHashMap<>(0));
    }

    /**
     * Ctor.
     * @param oby The objectionary
     * @param che The cache
     */
    OyCached(final Objectionary oby, final Map<String, Input> che) {
        this.origin = oby;
        this.cache = che;
    }

    @Override
    public Input get(final String name) throws IOException {
        return this.cache.computeIfAbsent(
            name, key -> {
                try {
                    return this.origin.get(name);
                } catch (final IOException exception) {
                    throw new IllegalStateException(
                        "An error occurred during the access to the origin objectionary",
                        exception
                    );
                }
            }
        );
    }

    @Override
    public boolean contains(final String name) throws IOException {
        return this.cache.containsKey(name) || this.origin.contains(name);
    }
}
