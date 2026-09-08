/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

/**
 * A formation whose tail self-calls run as a loop.
 *
 * <p>The transpiler wraps into it every copy of a formation that
 * {@code recursion-to-loop.xsl} marked as a loop, the way {@link PhSticky}
 * wraps a pure one. An operation on the formation — dataization, a lookup
 * that falls through to φ, normalization — runs as usual until it forces a
 * {@link PhAgain}; the {@link ExAgain} it throws carries the next copy of
 * the formation, and this object repeats the same operation on the body of
 * that copy, its φ, in the same Java frame. The chain ends when an
 * iteration completes without a signal. The body that completed is
 * remembered, so a later operation on this copy jumps to it instead of
 * walking the chain again.</p>
 *
 * <p>Attributes of the formation itself — its voids, ρ, φ — are answered by
 * the formation directly and never reach a signal, since
 * {@link PhDefault#take(String)} walks φ only for a name the formation
 * lacks. That is also how the body of the next copy is taken here without
 * the loop of its own: {@code take("φ")} on a copy answers its φ
 * unforced.</p>
 *
 * @since 0.76
 */
public final class PhLoop implements Phi {

    /**
     * The formation.
     */
    private final Phi origin;

    /**
     * The body that completed, once one did.
     */
    private final AtomicReference<Phi> base;

    /**
     * Ctor.
     * @param phi The formation
     */
    public PhLoop(final Phi phi) {
        this.origin = phi;
        this.base = new AtomicReference<>();
    }

    @Override
    public Phi copy() {
        return new PhLoop(this.origin.copy());
    }

    @Override
    public boolean needsRho() {
        return this.origin.needsRho();
    }

    @Override
    public Phi take(final String name) {
        return this.through(phi -> phi.take(name));
    }

    @Override
    public void put(final int position, final Phi object) {
        this.origin.put(position, object);
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
    public Phi normalized() {
        return this.through(Phi::normalized);
    }

    @Override
    public byte[] delta() {
        return this.through(Phi::delta);
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    private <T> T through(final Function<Phi, T> action) {
        Phi cur = this.origin;
        boolean own = true;
        while (true) {
            try {
                final T result = action.apply(cur);
                if (!own) {
                    this.base.compareAndSet(null, cur);
                }
                return result;
            } catch (final ExAgain again) {
                final Phi done = this.base.get();
                if (own && done != null) {
                    cur = done;
                } else {
                    cur = again.next().take(Phi.PHI);
                }
                own = false;
            }
        }
    }
}
