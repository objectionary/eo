/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * The attribute that copies the object and binds itself as its \rho, but
 * only when the object declares a \rho and has not been bound to one yet.
 * The terminator ({@link PhTerminator}) silently ignores this \rho itself, so no container
 * leaks into it and its cause is not masked as it propagates.
 * The copy it makes is kept ({@link WithRho}), so that every caller, on its own thread
 * or not, takes the very same object for as long as the one being bound does not change.
 * @since 0.36.0
 */
final class AtWithRho implements Attribute {

    /**
     * Original attribute.
     */
    private final Attribute original;

    /**
     * Rho.
     */
    private final Phi rho;

    /**
     * The copy that carries the rho, once it is made.
     */
    private final AtomicReference<WithRho> bound;

    /**
     * Lock guarding the making of the copy.
     */
    private final Lock lock;

    /**
     * Ctor.
     * @param attr Attribute
     * @param rho Rho
     */
    AtWithRho(final Attribute attr, final Phi rho) {
        this.original = attr;
        this.rho = rho;
        this.bound = new AtomicReference<>(null);
        this.lock = new ReentrantLock();
    }

    @Override
    public Attribute copy(final Phi self) {
        return new AtWithRho(
            this.original.copy(self),
            self
        );
    }

    @Override
    public Phi get() {
        Phi ret = this.original.get();
        if (ret.needsRho()) {
            this.lock.lock();
            try {
                final WithRho previous = this.bound.get();
                if (previous == null || !previous.made(ret)) {
                    final Phi copy = ret.copy();
                    copy.put(Phi.RHO, this.rho);
                    this.bound.set(new WithRho(ret, copy));
                }
                ret = this.bound.get().phi();
            } finally {
                this.lock.unlock();
            }
        }
        return ret;
    }

    @Override
    public void put(final Phi phi) {
        this.original.put(phi);
    }

    @Override
    public boolean vacant() {
        return this.original.vacant();
    }

    @Override
    public String φTerm() {
        return this.original.φTerm();
    }
}
