/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Cached Phi.
 *
 * <p>It's highly recommended to use it with {@link PhComposite}.</p>
 *
 * @since 0.1
 */
public final class PhCached implements Phi {

    /**
     * Origin attribute.
     */
    private final Phi origin;

    /**
     * Cache.
     */
    private final AtomicReference<Phi> cached;

    /**
     * Reentrant lock for thread-safe cache initialization.
     */
    private final ReentrantLock lock;

    /**
     * Ctor.
     * @param attr Origin attribute
     */
    public PhCached(final Phi attr) {
        this.origin = attr;
        this.cached = new AtomicReference<>();
        this.lock = new ReentrantLock();
    }

    @Override
    public Phi copy() {
        return this.origin.copy();
    }

    @Override
    public boolean hasRho() {
        return this.origin.hasRho();
    }

    @Override
    public Phi take(final String name) {
        Phi result = this.cached.get();
        if (result == null) {
            this.lock.lock();
            try {
                result = this.cached.get();
                if (result == null) {
                    result = this.origin.take(name);
                    this.cached.set(result);
                }
            } finally {
                this.lock.unlock();
            }
        }
        return result;
    }

    @Override
    public void put(final int pos, final Phi object) {
        throw new ExReadOnly(
            String.format(
                "Can't overwrite the \"%s\" attribute",
                this.origin
            )
        );
    }

    @Override
    public void put(final String name, final Phi object) {
        throw new ExReadOnly(
            String.format(
                "Can't overwrite the \"%s\" attribute",
                this.origin
            )
        );
    }

    @Override
    public String locator() {
        return this.origin.locator();
    }

    @Override
    public String forma() {
        return this.origin.forma();
    }

    @Override
    public byte[] delta() {
        return this.origin.delta();
    }
}
