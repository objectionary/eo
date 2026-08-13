/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * The bottom object of φ-calculus — a terminated computation.
 *
 * <p>It is a value that can be carried around — returned, copied, and
 * have an object put into it — but it has no data and no behaviour.
 * It detonates only when something tries to <em>force</em> it: reading
 * its data ({@link #delta()}) aborts through an {@link ExFailure}, which
 * nothing intercepts, so forcing bottom terminates the program for good.</p>
 *
 * <p>The remaining operations are tolerant on purpose: {@link #copy()}
 * yields the same bottom and {@link #take(String)} yields another one carrying
 * the same reason, so it propagates through copying and dispatch and surfaces
 * the failure at the outer dataization, not at the point it was produced.</p>
 *
 * <p>A bottom may carry a <em>cause</em>: it has a single slot, addressable only
 * at position 0 (as in {@code T "why it failed"}). Only a bottom without a cause
 * listens to that slot, and a dispatch hands one to the bottom it yields, so the
 * arguments that follow a propagated bottom, as in {@code (T).if a b}, are
 * dropped instead of being read as the reason it terminated. A {@code put} by
 * name other than ρ aborts — bottom has no named attributes. The ρ-binding the
 * runtime attempts on every take (via {@link AtWithRho}) is silently ignored,
 * since a bottom has no ρ; this keeps its cause from being masked by a
 * ρ-rejection while it propagates. The cause is write-once and never handed back
 * by {@link #take(String)}, so EO code can neither read it nor catch it — it
 * exists only to explain the termination at the very top.</p>
 *
 * @since 0.73.1
 */
public final class PhTerminator implements Phi {

    /**
     * The reason this computation terminated, used only as the panic
     * message when the bottom is forced, or {@code null} when none was given.
     */
    private Phi cause;

    /**
     * Ctor.
     */
    public PhTerminator() {
        // nothing
    }

    /**
     * Make a bottom that already carries the given reason as its cause.
     *
     * <p>The reason is remembered and used as the panic message when this bottom
     * is finally forced; until then it flows like any other bottom.</p>
     *
     * @param cause The reason for the termination
     * @return A bottom carrying the cause
     */
    @SuppressWarnings("PMD.ProhibitPublicStaticMethods")
    public static PhTerminator withCause(final String cause) {
        final PhTerminator term = new PhTerminator();
        term.put(0, new Data.ToPhi(cause));
        return term;
    }

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
        final PhTerminator term = new PhTerminator();
        term.put(0, this.reason());
        return term;
    }

    @Override
    public void put(final int pos, final Phi object) {
        if (pos == 0 && this.cause == null) {
            this.cause = object;
        }
    }

    @Override
    public void put(final String name, final Phi object) {
        if (!Phi.RHO.equals(name)) {
            throw new ExFailure(
                "the ⊥ object does not accept attributes by name, but got '%s'", name
            );
        }
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
        throw new ExFailure("%s", new Dataized(this.reason()).asString());
    }

    @Override
    public Phi normalized() {
        return this;
    }

    @Override
    public String φTerm() {
        return "⊥";
    }

    /**
     * The reason to panic with, either the cause this bottom was given or the
     * plain explanation of a bottom that was born without one.
     * @return The reason as an object
     */
    private Phi reason() {
        final Phi reason;
        if (this.cause == null) {
            reason = new Data.ToPhi("the ⊥ object is a terminated computation and cannot be used");
        } else {
            reason = this.cause;
        }
        return reason;
    }
}
