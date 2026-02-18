/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Concurrent cache wrapper for Cache.
 * Wrap {@link Cache} to make it thread-safe.
 * @since 0.60
 */
final class ConcurrentCache {

    /**
     * Original cache.
     */
    private final Cache original;

    /**
     * Locks for each cache entry.
     */
    private final ConcurrentMap<Path, Lock> locks;

    /**
     * Ctor.
     * @param original Original cache
     */
    ConcurrentCache(final Cache original) {
        this(original, new ConcurrentHashMap<>(0));
    }

    /**
     * Ctor.
     * @param original Original cache
     * @param locks Locks map
     */
    private ConcurrentCache(final Cache original, final ConcurrentMap<Path, Lock> locks) {
        this.original = original;
        this.locks = locks;
    }

    /**
     * Check cache and apply compilation if needed.
     * @param source From file
     * @param target To file
     * @param tail Tail path in cache
     */
    void apply(final Path source, final Path target, final Path tail) {
        final Lock loc = this.locks.computeIfAbsent(tail.normalize(), k -> new ReentrantLock());
        loc.lock();
        try {
            this.original.apply(source, target, tail.normalize());
        } finally {
            loc.unlock();
        }
    }
}
