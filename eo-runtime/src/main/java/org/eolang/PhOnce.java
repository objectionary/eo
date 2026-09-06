/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Supplier;

/**
 * An object wrapping another one.
 *
 * <p>It never needs a receiver. A receiver is bound onto a formation, never
 * onto the result of an expression: the dispatch that produced this object
 * has already given it the receiver it deserves. Saying so without forcing
 * the wrapped object keeps a lazy expression lazy while it is dispatched
 * over.</p>
 *
 * <p>An object of this class is equal to itself and to nothing else, the
 * way {@code PhDefault} is. Answering on behalf of the wrapped object
 * would make the answer one-sided: the decorator would say it equals the
 * object it wraps, while that object says it does not equal the decorator,
 * and {@code Object.equals} requires the two to agree.</p>
 *
 * @since 0.1
 * @todo #7062:30min Stop passing a null term through {@link PhOnce#wrapped}.
 *  The one-argument constructor still passes {@code null} to mean "render the
 *  wrapped object instead", the term method branches on it, and {@code copy()}
 *  and {@code normalized()} now hand that same {@code null} to every subclass
 *  across a protected extension point, so each of them has to tolerate it. An
 *  empty term object would remove the branch and the null alike.
 */
public abstract class PhOnce implements Phi {

    /**
     * The object fetched.
     */
    private final Supplier<Phi> object;

    /**
     * Reference.
     */
    private final AtomicReference<Phi> ref;

    /**
     * Supplier of the φ-term, or {@code null} to delegate to the wrapped object.
     */
    private final Supplier<String> term;

    /**
     * Lock guarding the first load of the reference.
     */
    private final Lock lock;

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
        this.term = term;
        this.lock = new ReentrantLock();
        this.object = () -> this.loaded(obj);
    }

    @Override
    public final boolean equals(final Object obj) {
        return this == obj;
    }

    @Override
    public final int hashCode() {
        return System.identityHashCode(this) + 1;
    }

    @Override
    public final Phi copy() {
        return this.wrapped(() -> this.object.get().copy(), this.term);
    }

    @Override
    public final boolean needsRho() {
        return false;
    }

    @Override
    public final Phi take(final String name) {
        return this.object.get().take(name);
    }

    @Override
    public final void put(final int pos, final Phi obj) {
        this.object.get().put(pos, obj);
    }

    @Override
    public final void put(final String name, final Phi obj) {
        this.object.get().put(name, obj);
    }

    @Override
    public final String locator() {
        return this.object.get().locator();
    }

    @Override
    public final String forma() {
        return this.object.get().forma();
    }

    @Override
    public final byte[] delta() {
        return this.object.get().delta();
    }

    @Override
    public final Phi normalized() {
        final Phi result = this.object.get().normalized();
        final Phi normalized;
        if (result instanceof PhTerminator) {
            normalized = result;
        } else {
            normalized = this.wrapped(() -> result, this.term);
        }
        return normalized;
    }

    @Override
    public final String φTerm() {
        final String result;
        if (this.term == null) {
            result = this.object.get().φTerm();
        } else {
            result = this.term.get();
        }
        return result;
    }

    /**
     * Wrap the given object into an object of the same type as this one.
     * @param obj The object to wrap
     * @param phrase Supplier of the φ-term
     * @return The wrapper
     */
    protected abstract Phi wrapped(Supplier<Phi> obj, Supplier<String> phrase);

    private Phi loaded(final Supplier<Phi> obj) {
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
    }
}
