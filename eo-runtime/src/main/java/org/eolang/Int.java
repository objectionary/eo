/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Transform {@link Expect} to Integer.
 *
 * <p>Public because the syscall adapters, which live in another package,
 * map EO numbers onto C {@code int} parameters and have to refuse the
 * numbers no such parameter can mean, the way {@link Natural} refuses the
 * sizes.</p>
 *
 * @since 0.51
 */
public final class Int {

    /**
     * Expect.
     */
    private final Expect<Phi> expect;

    /**
     * Ctor.
     *
     * @param subject What the number is, for the failure message
     * @param phi The object holding the number
     */
    public Int(final String subject, final Phi phi) {
        this(new Expect<>(subject, () -> phi));
    }

    /**
     * Ctor.
     *
     * @param expect Expect
     */
    public Int(final Expect<Phi> expect) {
        this.expect = expect;
    }

    /**
     * Return it.
     *
     * @return The token
     */
    public Integer it() {
        return this.expect
            .that(phi -> new Dataized(phi).asNumber())
            .otherwise("must be a number")
            .must(number -> number % 1 == 0)
            .otherwise("must be an integer")
            .must(number -> number >= Integer.MIN_VALUE && number <= Integer.MAX_VALUE)
            .otherwise("must fit into int range")
            .that(Double::intValue)
            .it();
    }
}
