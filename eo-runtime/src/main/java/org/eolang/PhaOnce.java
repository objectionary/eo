/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;

/**
 * Attribute that retrieves object only once.
 *
 * <p>It's highly recommended to use it with {@link PhComposite}.</p>
 *
 * @since 0.1
 */
public final class PhaOnce implements Phi {

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
    public PhaOnce(final Phi attr) {
        this.origin = attr;
        this.cached = new AtomicReference<>();
    }

    @Override
    public Phi copy(final Phi self) {
        return new PhaOnce(this.origin.copy(self));
    }

    @Override
    public Phi copy() {
        throw new UnsupportedOperationException("#copy()");
    }

    @Override
    public boolean hasRho() {
        throw new UnsupportedOperationException("#hasRho()");
    }

    @Override
    public Phi take(final String name) {
        synchronized (this.cached) {
            if (this.cached.get() == null) {
                this.cached.set(this.origin.take(0));
            }
        }
        return this.cached.get();
    }

    @Override
    public Phi take(final int pos) {
        synchronized (this.cached) {
            if (this.cached.get() == null) {
                this.cached.set(this.origin.take(0));
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
        throw new UnsupportedOperationException("#locator()");
    }

    @Override
    public String forma() {
        throw new UnsupportedOperationException("#forma()");
    }

    @Override
    public byte[] delta() {
        throw new UnsupportedOperationException("#delta()");
    }
}
