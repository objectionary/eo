/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicReference;

/**
 * The terminator of φ-calculus — a terminated computation.
 *
 * <p>It is a value that can be carried around — returned, copied, and
 * have an object put into it — but it has no data and no behaviour.
 * It detonates only when something tries to <em>force</em> it: reading
 * its data ({@link #delta()}) aborts through an {@link ExFailure}, which
 * only {@link EOrecovered} intercepts, and only while it resolves its own
 * {@code value}, so forcing a terminator anywhere else terminates the program
 * for good.</p>
 *
 * <p>The remaining operations are tolerant on purpose: {@link #copy()}
 * yields the same terminator and {@link #take(String)} yields another one carrying
 * the same reason, so it propagates through copying and dispatch and surfaces
 * the failure at the outer dataization, not at the point it was produced.</p>
 *
 * <p>A terminator may carry a <em>cause</em>: it has a single slot, addressable only
 * at position 0 (as in {@code T "why it failed"}). Only a terminator without a cause
 * listens to that slot, and a dispatch hands one to the terminator it yields, so the
 * arguments that follow a propagated terminator, as in {@code (T).if a b}, are
 * dropped instead of being read as the reason it terminated. A {@code put} by
 * name other than ρ aborts — a terminator has no named attributes. The ρ-binding the
 * runtime attempts on every take (via {@link AtWithRho}) is silently ignored,
 * since a terminator has no ρ; this keeps its cause from being masked by a
 * ρ-rejection while it propagates. The cause is write-once and never handed back
 * by {@link #take(String)}, so EO code can neither read it nor catch it — it
 * exists only to explain the termination at the very top.</p>
 *
 * @since 0.73.1
 */
public final class PhTerminator implements Phi {

    /**
     * The reason used when none is given at birth.
     */
    private static final String DEFAULT =
        "the ⊥ object is a terminated computation and cannot be used";

    /**
     * The reason this computation terminated, used only as the panic
     * message when the terminator is forced, empty when none was given.
     * Atomic because {@link #copy()} hands the same terminator to every
     * branch of a concurrent evaluation: the write-once rule the class
     * promises has to hold when two of those branches put at once, and the
     * cause one of them stores has to be visible to the one that forces it.
     */
    private final AtomicReference<Phi> cause;

    /**
     * The reason to fall back to if nothing is ever {@code put} into this
     * terminator. Unlike {@link #cause}, a birth-site default never blocks a
     * later, more specific {@code put} — a caller that takes a void
     * attribute's terminator and immediately puts its own reason (as
     * {@code bytes.slice} does with its {@code cant-slice} fallback) must
     * still win.
     */
    private final Phi fallback;

    /**
     * Ctor.
     */
    public PhTerminator() {
        this(null, PhTerminator.DEFAULT);
    }

    /**
     * Make a terminator that explains, by default, why it was born without a
     * dispatch ever reaching it, while still letting a caller that takes it
     * and puts its own, more specific reason override that default.
     *
     * @param reason The default reason for the termination
     */
    public PhTerminator(final String reason) {
        this(null, reason);
    }

    /**
     * Make a terminator that already carries the given reason as its cause.
     *
     * <p>The reason is remembered and used as the panic message when this
     * terminator is finally forced; until then it flows like any other one.</p>
     *
     * @param cause The reason for the termination
     */
    public PhTerminator(final Phi cause) {
        this(cause, PhTerminator.DEFAULT);
    }

    /**
     * Primary ctor.
     *
     * @param cse The cause already carried, or {@code null} for none
     * @param reason The default reason for the termination
     */
    private PhTerminator(final Phi cse, final String reason) {
        this.cause = new AtomicReference<>(cse);
        this.fallback = new Data.ToPhi(reason);
    }

    @Override
    public Phi copy() {
        return this;
    }

    @Override
    public boolean needsRho() {
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
        if (pos == 0) {
            this.cause.compareAndSet(null, object);
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

    private Phi reason() {
        final Phi reason;
        final Phi carried = this.cause.get();
        if (carried != null) {
            reason = carried;
        } else {
            reason = this.fallback;
        }
        return reason;
    }
}
