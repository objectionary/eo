/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Supplier;

/**
 * An object wrapping another one.
 * @since 0.1
 * @checkstyle DesignForExtensionCheck (200 lines)
 */
@SuppressWarnings("PMD.TooManyMethods")
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
     * Lock for thread-safe initialization.
     */
    private final ReentrantLock lock;

    /**
     * Supplier of the φ-term, or {@code null} to delegate to the wrapped object.
     */
    private final Supplier<String> term;

    /**
     * Ctor.
     * @param obj The object
     */
    public PhOnce(final Supplier<Phi> obj) {
        this(obj, null);
    }

    /**
     * Ctor.
     * @param obj The object
     * @param term Supplier of the φ-term
     */
    public PhOnce(final Supplier<Phi> obj, final Supplier<String> term) {
        this.ref = new AtomicReference<>(null);
        this.lock = new ReentrantLock();
        this.term = term;
        this.object = () -> {
            if (this.ref.get() == null) {
                this.lock.lock();
                try {
                    if (this.ref.get() == null) {
                        this.ref.set(obj.get());
                    }
                } finally {
                    this.lock.unlock();
                }
            }
            return this.ref.get();
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
            () -> this.object.get().copy(),
            this.term
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

    @Override
    public Phi normalized() {
        return this.object.get().normalized();
    }

    @Override
    public String φTerm() {
        final String result;
        if (this.term == null) {
            result = this.object.get().φTerm();
        } else {
            result = this.term.get();
        }
        return result;
    }
}
