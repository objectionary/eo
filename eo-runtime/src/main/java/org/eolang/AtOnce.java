/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Attribute that retrieves object only once.
 *
 * <p>It's highly recommended to use it with {@link AtComposite}.</p>
 *
 * @since 0.1
 */
public final class AtOnce implements Attr {

    /**
     * Origin attribute.
     */
    private final Attr origin;

    /**
     * Cache.
     */
    private final AtomicReference<Phi> cached;

    /**
     * Reentrant lock for thread-safe initialization.
     */
    private final ReentrantLock lock;

    /**
     * Ctor.
     * @param attr Origin attribute
     */
    public AtOnce(final Attr attr) {
        this.origin = attr;
        this.cached = new AtomicReference<>(null);
        this.lock = new ReentrantLock();
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtOnce(this.origin.copy(self));
    }

    @Override
    public Phi get() {
        Phi result = this.cached.get();
        if (result == null) {
            this.lock.lock();
            try {
                result = this.cached.get();
                if (result == null) {
                    result = this.origin.get();
                    this.cached.set(result);
                }
            } finally {
                this.lock.unlock();
            }
        }
        return result;
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
}
