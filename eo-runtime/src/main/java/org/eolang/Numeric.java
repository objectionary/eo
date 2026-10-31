/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Transform {@link Expect} to a floating-point number.
 *
 * @since 0.51
 */
final class Numeric {

    /**
     * Expect.
     */
    private final Expect<Phi> expect;

    /**
     * Ctor.
     *
     * @param expect Expect
     */
    Numeric(final Expect<Phi> expect) {
        this.expect = expect;
    }

    /**
     * Return it.
     *
     * @return The token
     */
    Double it() {
        return this.expect
            .that(phi -> new Dataized(phi).asNumber())
            .otherwise("must be a number")
            .it();
    }
}
