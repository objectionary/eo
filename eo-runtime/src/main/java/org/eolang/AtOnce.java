/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;

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
     * Ctor.
     * @param attr Origin attribute
     */
    public AtOnce(final Attr attr) {
        this.origin = attr;
        this.cached = new AtomicReference<>();
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtOnce(this.origin.copy(self));
    }

    @Override
    public Phi get() {
        synchronized (this.cached) {
            if (this.cached.get() == null) {
                this.cached.set(this.origin.get());
            }
        }
        return this.cached.get();
    }

    @Override
    public void put(final Phi phi) {
        throw new ExReadOnly(
            String.format(
                "Can't overwrite the \"%s\" attribute",
                this.origin
            )
        );
    }
}
