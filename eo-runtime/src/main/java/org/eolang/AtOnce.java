/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Attribute that retrieves object only once.
 *
 * <p>It's highly recommended to use it with {@link AtComposite}.</p>
 *
 * @since 0.1
 */
public final class AtOnce implements Attribute {

    /**
     * Origin attribute.
     */
    private final Attribute origin;

    /**
     * Cache.
     */
    private final AtomicReference<Phi> cached;

    /**
     * Lock guarding the first retrieval of the origin attribute.
     */
    private final Lock lock;

    /**
     * Ctor.
     *
     * @param attr Origin attribute
     */
    public AtOnce(final Attribute attr) {
        this.origin = attr;
        this.cached = new AtomicReference<>(null);
        this.lock = new ReentrantLock();
    }

    @Override
    public Attribute copy(final Phi self) {
        return new AtOnce(this.origin.copy(self));
    }

    @Override
    public Phi get() {
        if (this.cached.get() == null) {
            this.lock.lock();
            try {
                if (this.cached.get() == null) {
                    this.cached.set(this.origin.get());
                }
            } finally {
                this.lock.unlock();
            }
        }
        return this.cached.get();
    }

    @Override
    public void put(final Phi phi) {
        throw new ExReadOnly(
            String.format(
                "Can't overwrite the cached attribute \"%s\"",
                this.origin
            )
        );
    }

    @Override
    public boolean vacant() {
        return false;
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }
}
