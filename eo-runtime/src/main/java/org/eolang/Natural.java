/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Transform {@link Expect} to Natural number.
 * Natural number is integer greater or equal to zero.
 *
 * @since 0.51
 */
public final class Natural {

    /**
     * Expect.
     */
    private final Expect<Phi> expect;

    /**
     * Ctor.
     *
     * @param expect Expect
     */
    public Natural(final Expect<Phi> expect) {
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
            .must(integer -> integer >= 0)
            .otherwise("must be greater or equal to zero")
            .it();
    }
}
