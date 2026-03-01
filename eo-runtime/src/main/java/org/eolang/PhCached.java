/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;

/**
 * Cached Phi.
 *
 * <p>It's highly recommended to use it with {@link PhComposite}.</p>
 *
 * @since 0.1
 * @todo #4884:30min Replace 'synchronized' with ReentrantLock.
 *  We need to replace 'synchronized' with ReentrantLock to avoid potential
 *  deadlocks when multiple threads are trying to access the cache simultaneously.
 *  Moreover, 'synchronized' keyword is forbidden by qulice.
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
     * Ctor.
     * @param attr Origin attribute
     */
    public PhCached(final Phi attr) {
        this.origin = attr;
        this.cached = new AtomicReference<>();
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
    @SuppressWarnings("PMD.AvoidSynchronizedStatement")
    public Phi take(final String name) {
        synchronized (this.cached) {
            if (this.cached.get() == null) {
                this.cached.set(this.origin.take(name));
            }
        }
        return this.cached.get();
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
