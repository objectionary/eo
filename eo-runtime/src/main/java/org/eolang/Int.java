/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Transform {@link Expect} to Integer.
 * @since 0.51
 */
final class Int {

    /**
     * Expect.
     */
    private final Expect<Phi> expect;

    /**
     * Ctor.
     * @param expect Expect
     */
    Int(final Expect<Phi> expect) {
        this.expect = expect;
    }

    /**
     * Return it.
     * @return The token
     */
    Integer it() {
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
