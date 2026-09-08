/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * A tail call of a looped formation.
 *
 * <p>The transpiler wraps into it every self-call standing in a tail
 * position of a formation it marked as a loop (see
 * {@code recursion-to-loop.xsl} in eo-maven-plugin). Any attempt to force
 * the call — an attribute lookup, normalization, dataization — throws
 * {@link ExAgain} carrying the call, so that the {@link PhLoop} around the
 * formation continues with it in place. Everything that does not force the
 * call is delegated to it.</p>
 *
 * @since 0.76
 */
public final class PhAgain implements Phi {

    /**
     * The tail call.
     */
    private final Phi next;

    /**
     * Ctor.
     * @param phi The tail call
     */
    public PhAgain(final Phi phi) {
        this.next = phi;
    }

    @Override
    public Phi copy() {
        return new PhAgain(this.next.copy());
    }

    @Override
    public boolean needsRho() {
        return this.next.needsRho();
    }

    @Override
    public Phi take(final String name) {
        throw new ExAgain(this.next);
    }

    @Override
    public void put(final int position, final Phi object) {
        this.next.put(position, object);
    }

    @Override
    public void put(final String name, final Phi object) {
        this.next.put(name, object);
    }

    @Override
    public String locator() {
        return this.next.locator();
    }

    @Override
    public String forma() {
        return this.next.forma();
    }

    @Override
    public Phi normalized() {
        throw new ExAgain(this.next);
    }

    @Override
    public byte[] delta() {
        throw new ExAgain(this.next);
    }

    @Override
    public String φTerm() {
        return this.next.φTerm();
    }
}
