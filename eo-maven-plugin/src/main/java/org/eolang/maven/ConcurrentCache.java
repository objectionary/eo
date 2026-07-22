/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Concurrent guard for {@link Cache}.
 *
 * <p>Serializes writes to the same cache location so that {@link Cache}, which
 * isn't thread-safe on its own, can be used from a {@link Threaded} pool. The
 * lock is picked from a shared map keyed by the cache "tail" path, so two
 * threads compiling the same file contend on the same lock. For that to work a
 * <em>single</em> instance must be shared across all files of one Mojo run:
 * a fresh instance per file would hand each thread its own empty lock map and
 * defeat the serialization entirely (#5720). The per-file {@link Cache} itself
 * is passed in to {@link #apply}, since it differs from file to file.</p>
 *
 * @since 0.60
 */
final class ConcurrentCache {

    /**
     * Locks for each cache entry, shared across every file of one run.
     */
    private final ConcurrentMap<Path, ReentrantLock> locks;

    /**
     * Ctor.
     */
    ConcurrentCache() {
        this(new ConcurrentHashMap<>(0));
    }

    /**
     * Ctor.
     * @param shared Locks map
     */
    private ConcurrentCache(final ConcurrentMap<Path, ReentrantLock> shared) {
        this.locks = shared;
    }

    /**
     * Check cache and apply compilation if needed, holding the lock for the
     * given tail path so concurrent writes to it are serialized.
     * @param cache The per-file cache to run under the lock
     * @param source From file
     * @param target To file
     * @param tail Tail path in cache
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    void apply(final Cache cache, final Path source, final Path target, final Path tail) {
        final ReentrantLock lock = this.locks.computeIfAbsent(
            tail.normalize(), k -> new ReentrantLock()
        );
        lock.lock();
        try {
            cache.apply(source, target, tail.normalize());
        } finally {
            lock.unlock();
        }
    }
}
