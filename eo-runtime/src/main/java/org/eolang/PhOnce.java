/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

/**
 * An object wrapping another one.
 *
 * @since 0.1
 * @todo #4884:30min Use ReentrantLock instead of synchronized block in the constructor.
 *  This will allow to avoid blocking the whole object while fetching the wrapped one.
 *  Moreover, using 'syznchronized' is forbidden by qulice.
 *  Don't forget to remove the suppression of PMD.AvoidSynchronizedStatement in
 *  the constructor after that.
 * @checkstyle DesignForExtensionCheck (100 lines)
 */
public class PhOnce implements Phi {

    /**
     * The object fetched.
     */
    private final Supplier<Phi> object;

    /**
     * Reference.
     */
    private final AtomicReference<Phi> ref;

    /**
     * Ctor.
     *
     * @param obj The object
     */
    @SuppressWarnings("PMD.AvoidSynchronizedStatement")
    public PhOnce(final Supplier<Phi> obj) {
        this.ref = new AtomicReference<>(null);
        this.object = () -> {
            synchronized (this.ref) {
                if (this.ref.get() == null) {
                    this.ref.set(obj.get());
                }
                return this.ref.get();
            }
        };
    }

    @Override
    public boolean equals(final Object obj) {
        return this.object.get().equals(obj);
    }

    @Override
    public int hashCode() {
        return this.object.get().hashCode();
    }

    @Override
    public Phi copy() {
        return new PhOnce(
            () -> this.object.get().copy()
        );
    }

    @Override
    public boolean hasRho() {
        return this.object.get().hasRho();
    }

    @Override
    public Phi take(final String name) {
        return this.object.get().take(name);
    }

    @Override
    public void put(final int pos, final Phi obj) {
        this.object.get().put(pos, obj);
    }

    @Override
    public void put(final String name, final Phi obj) {
        this.object.get().put(name, obj);
    }

    @Override
    public String locator() {
        return this.object.get().locator();
    }

    @Override
    public String forma() {
        return this.object.get().forma();
    }

    @Override
    public byte[] delta() {
        return this.object.get().delta();
    }
}
