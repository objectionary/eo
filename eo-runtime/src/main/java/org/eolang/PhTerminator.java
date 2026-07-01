/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * The ⊥ ("bottom") object of φ-calculus — a terminated computation.
 *
 * <p>It is a value that can be carried around — returned, copied, and
 * have attributes bound into it — but it has no data and no behaviour.
 * It detonates only when something tries to <em>force</em> it: reading
 * its data ({@link #delta()}) or dispatching an attribute on it
 * ({@link #take(String)}) aborts through an {@link ExFailure}. Because
 * EO {@code try} only intercepts {@link EOerror.ExError}, that failure
 * cannot be caught, so forcing ⊥ terminates the program for good.</p>
 *
 * <p>The remaining operations are tolerant on purpose: {@link #copy()}
 * yields another ⊥, {@link #put(int, Phi)} is a no-op, and the
 * metadata accessors return sentinels. This lets ⊥ propagate through
 * copying and ρ-binding (the dispatch plumbing) and surface the failure
 * at the outer dataization or dispatch, not at the point it was
 * produced.</p>
 *
 * @since 0.73.1
 */
public final class PhTerminator implements Phi {

    @Override
    public Phi copy() {
        return this;
    }

    @Override
    public boolean hasRho() {
        return false;
    }

    @Override
    public Phi take(final String name) {
        return this;
    }

    @Override
    public void put(final int pos, final Phi object) {
        // No-op: ⊥ has no attributes; binding into it is ignored so the
        // failure surfaces only when ⊥ is forced (delta/take).
    }

    @Override
    public void put(final String name, final Phi object) {
        // No-op: ⊥ has no attributes; binding into it is ignored so the
        // failure surfaces only when ⊥ is forced (delta/take).
    }

    @Override
    public String locator() {
        return "?";
    }

    @Override
    public String forma() {
        return "⊥";
    }

    @Override
    public byte[] delta() {
        throw new ExFailure("the ⊥ object is a terminated computation and cannot be used");
    }

    @Override
    public String φTerm() {
        return "⊥";
    }
}
