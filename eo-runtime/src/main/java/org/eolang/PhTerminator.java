/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * The ⊥ ("bottom") object of φ-calculus — a terminated computation.
 *
 * <p>It is a value that can be carried around — returned, copied, and
 * have an object put into it — but it has no data and no behaviour.
 * It detonates only when something tries to <em>force</em> it: reading
 * its data ({@link #delta()}) aborts through an {@link ExFailure}. Because
 * EO {@code try} only intercepts {@link EOerror.ExError}, that failure
 * cannot be caught, so forcing ⊥ terminates the program for good.</p>
 *
 * <p>The remaining operations are tolerant on purpose: {@link #copy()}
 * yields the same ⊥ and {@link #take(String)} yields ⊥ again, so it
 * propagates through copying and dispatch and surfaces the failure at
 * the outer dataization, not at the point it was produced.</p>
 *
 * <p>A ⊥ may carry a <em>cause</em>: it has a single slot, addressable at
 * position 0 or as the attribute {@code cause} (as in {@code T "why it
 * failed"}), and any other {@code put} aborts. The first object put there
 * is remembered and used as the panic message when the ⊥ is finally forced.
 * The cause is write-once and never handed back by {@link #take(String)},
 * so EO code can neither read it nor catch it — it exists only to explain
 * the termination at the very top.</p>
 *
 * @since 0.73.1
 */
public final class PhTerminator implements Phi {

    /**
     * The reason this computation terminated, used only as the panic
     * message when the ⊥ is forced, or {@code null} when none was given.
     */
    private Phi cause;

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
        if (pos != 0) {
            throw new ExFailure(
                "the ⊥ object only accepts a cause at position 0, not %d", pos
            );
        }
        this.remember(object);
    }

    @Override
    public void put(final String name, final Phi object) {
        if (!"cause".equals(name)) {
            throw new ExFailure(
                "the ⊥ object only accepts a cause named 'cause', not '%s'", name
            );
        }
        this.remember(object);
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
        final String reason;
        if (this.cause == null) {
            reason = "the ⊥ object is a terminated computation and cannot be used";
        } else {
            reason = new Dataized(this.cause).asString();
        }
        throw new ExFailure(reason);
    }

    @Override
    public String φTerm() {
        return "⊥";
    }

    /**
     * Remember the first object put into this ⊥ as its cause, ignoring any
     * later ones so the birth reason survives propagation.
     * @param object The object being put
     */
    private void remember(final Phi object) {
        if (this.cause == null) {
            this.cause = object;
        }
    }
}
