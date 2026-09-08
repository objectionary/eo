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
 * <p>Serializes writes to one cache location, so that {@link Cache}, which
 * isn't thread-safe, can be used from a {@link Threaded} pool. Locks come from
 * a map keyed by the cache "tail" path, so a <em>single</em> instance must be
 * shared by all files of a run: one per file hands each thread an empty map
 * and serializes nothing (#5720). The {@link Cache} goes to {@link #apply},
 * as it differs from file to file.</p>
 *
 * <p>Guards are per-instance and {@link Parsing} is built by both
 * {@link MjParse} and {@link MjSafe#assembling(TjsForeign)}, which is enough within one
 * module, as those mojos never overlap. Modules built in parallel still share
 * the machine-wide cache directory (#2857), which no in-process lock covers.</p>
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
     *
     * @param shared Locks map
     */
    private ConcurrentCache(final ConcurrentMap<Path, ReentrantLock> shared) {
        this.locks = shared;
    }

    /**
     * Check cache and apply compilation if needed, under the tail path lock.
     *
     * @param source From file
     * @param target To file
     * @param tail Tail path in cache
     * @param cache The per-file cache to run under the lock
     * @checkstyle ParameterNumberCheck (3 lines)
     */
    void apply(final Path source, final Path target, final Path tail, final Cache cache) {
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
