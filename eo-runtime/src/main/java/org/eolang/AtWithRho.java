/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.ReentrantLock;

/**
 * The attribute that tries to copy object and set \rho to it if it has not already set.
 *
 * @since 0.36.0
 */
final class AtWithRho implements Attr {
    /**
     * Original attribute.
     */
    private final Attr original;

    /**
     * Rho.
     */
    private final Phi rho;

    /**
     * Cached result of {@link #get()} to guarantee a consistent view across threads.
     */
    private final AtomicReference<Phi> cached;

    /**
     * Lock guarding the first initialization of {@link #cached}.
     */
    private final ReentrantLock lock;

    /**
     * Ctor.
     * @param attr Attribute
     * @param rho Rho
     */
    AtWithRho(final Attr attr, final Phi rho) {
        this.original = attr;
        this.rho = rho;
        this.cached = new AtomicReference<>();
        this.lock = new ReentrantLock();
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtWithRho(
            this.original.copy(self),
            self
        );
    }

    @Override
    public Phi get() {
        Phi ret = this.cached.get();
        if (ret == null) {
            ret = this.initialize();
        }
        return ret;
    }

    @Override
    public void put(final Phi phi) {
        this.cached.set(null);
        this.original.put(phi);
    }

    /**
     * Initialize the cached value under the lock, using double-checked locking.
     * @return Cached {@link Phi}
     */
    private Phi initialize() {
        this.lock.lock();
        try {
            Phi ret = this.cached.get();
            if (ret == null) {
                ret = this.withRho(this.original.get());
                this.cached.set(ret);
            }
            return ret;
        } finally {
            this.lock.unlock();
        }
    }

    /**
     * Return the given object with {@link Phi#RHO} attached, copying it if needed.
     * @param phi Original object
     * @return Object with {@code \rho} set
     */
    private Phi withRho(final Phi phi) {
        Phi ret = phi;
        if (!ret.hasRho()) {
            ret = ret.copy();
            ret.put(Phi.RHO, this.rho);
        }
        return ret;
    }
}
