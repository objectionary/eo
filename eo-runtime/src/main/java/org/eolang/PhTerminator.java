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
 * yields the same bottom and {@link #take(String)} yields bottom again, so it
 * propagates through copying and dispatch and surfaces the failure at
 * the outer dataization, not at the point it was produced.</p>
 *
 * <p>A bottom may carry a <em>cause</em>: it has a single slot, addressable only
 * at position 0 (as in {@code T "why it failed"}). A {@code put} at any other
 * position is ignored rather than rejected, because a propagated bottom answers
 * every take with itself and then receives the arguments of whatever dispatch
 * follows it: a two-argument one, such as {@code .if a b}, would otherwise abort
 * on its second argument and destroy the cause it was carrying. A {@code put} by
 * name other than ρ aborts — bottom has no named attributes. The ρ-binding the
 * runtime attempts on every take (via {@link AtWithRho}) is silently ignored,
 * since a bottom has no ρ; this keeps its cause from being masked by a
 * ρ-rejection while it propagates. The first object put at position 0 is
 * remembered and used as the panic message when the bottom is finally forced.
 * The cause is write-once and never handed back by {@link #take(String)}, so
 * EO code can neither read it nor catch it — it exists only to explain the
 * termination at the very top.</p>
 *
 * @since 0.73.1
 * @todo #6359:45min Keep a causeless bottom from adopting a dispatch argument.
 *  Position 0 is both the cause slot of {@code T "why"} and the first argument
 *  of whatever dispatch follows a propagated bottom, so {@code (T).if 1 2} takes
 *  the {@code 1} as its cause and panics with those bytes read as text, instead
 *  of saying that the computation was terminated.
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
        return this;
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
        final String reason;
        if (this.cause == null) {
            reason = "the ⊥ object is a terminated computation and cannot be used";
        } else {
            reason = new Dataized(this.cause).asString();
        }
        throw new ExFailure("%s", reason);
    }

    @Override
    public Phi normalized() {
        return this;
    }

    @Override
    public String φTerm() {
        return "⊥";
    }
}
