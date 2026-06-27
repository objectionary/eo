/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A decorator that memoizes the attributes it takes.
 *
 * <p>It wraps another object whose static φ-expression is baked in by the
 * transpiler as {@code key}. For a single targeted expression it remembers,
 * in a process-wide table, the result of {@link #take(String)} keyed on
 * {@code key.name}. A second taking of that same expression, anywhere in the
 * program, yields the very same {@link Phi} instead of building it again;
 * every other expression is delegated and rebuilt as usual.</p>
 *
 * <p>The key is a compile-time constant, so deciding whether to memoize
 * costs a single string comparison and never activates, dataizes nor
 * reflects upon the wrapped object. The transpiler bakes the object's
 * locator as the key; full purity gating (so that impure objects are never
 * shared) arrives with the {@code Impure} annotation in a later increment.</p>
 *
 * @since 0.74
 */
@SuppressWarnings("PMD.TooManyMethods")
public final class PhSticky implements Phi {

    /**
     * The single expression whose taking is memoized.
     */
    private static final String KEY = "Φ.nan.is-finite";

    /**
     * The table of memoized objects, shared across all stickies.
     */
    private static final Map<String, Phi> MEMORY = new ConcurrentHashMap<>(1);

    /**
     * The wrapped object.
     */
    private final Phi origin;

    /**
     * The static φ-expression of the wrapped object, baked by the transpiler.
     */
    private final String locator;

    /**
     * Ctor.
     * @param phi The object to wrap
     * @param loc The static φ-expression of the object
     */
    public PhSticky(final Phi phi, final String loc) {
        this.origin = phi;
        this.locator = loc;
    }

    @Override
    public boolean equals(final Object obj) {
        return this.origin.equals(obj);
    }

    @Override
    public int hashCode() {
        return this.origin.hashCode();
    }

    @Override
    public Phi copy() {
        return new PhSticky(this.origin.copy(), this.locator);
    }

    @Override
    public boolean hasRho() {
        return this.origin.hasRho();
    }

    @Override
    public Phi take(final String name) {
        final Phi taken;
        if (PhSticky.KEY.equals(String.join(".", this.locator, name))) {
            taken = PhSticky.MEMORY.computeIfAbsent(PhSticky.KEY, ignore -> this.origin.take(name));
        } else {
            taken = this.origin.take(name);
        }
        return taken;
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.origin.put(pos, object);
    }

    @Override
    public void put(final String name, final Phi object) {
        this.origin.put(name, object);
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

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }
}
