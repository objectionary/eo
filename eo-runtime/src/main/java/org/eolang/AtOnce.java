/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
     * Ctor.
     * @param attr Origin attribute
     */
    public AtOnce(final Attribute attr) {
        this.origin = attr;
        this.cached = new AtomicReference<>(null);
    }

    @Override
    public Attribute copy(final Phi self) {
        return new AtOnce(this.origin.copy(self));
    }

    @Override
    @SuppressWarnings({"PMD.AvoidSynchronizedStatement", "PMD.DoubleCheckedLocking"})
    public Phi get() {
        Phi result = this.cached.get();
        if (result == null) {
            synchronized (this.cached) {
                result = this.cached.get();
                if (result == null) {
                    result = this.origin.get();
                    this.cached.set(result);
                }
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

    @Override
    public boolean vacant() {
        return false;
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }
}
