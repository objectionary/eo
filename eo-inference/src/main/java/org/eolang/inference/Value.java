/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * One value the walk knows, named by what it is made of.
 *
 * <p>The kinds, by the letter that names them:</p>
 *
 * <ul>
 *   <li>{@code O}, a copy (3.1): its formation, its birthplace, the bits of
 *   its bound voids and its tag, empty when it has none;</li>
 *   <li>{@code Φ}, the root; {@code D}, a datum; {@code T}, a termination;
 *   {@code J}, what only Java puts into a {@code ρ};</li>
 *   <li>{@code U}, an unknown, with its reason in {@code form}, and
 *   "decided" in {@code birth} for an open void a stop decided (4.4);</li>
 *   <li>{@code X}, a failing read, with its label in {@code form};</li>
 *   <li>{@code B}, rule b's head as is (4.3), the value in {@code base};</li>
 *   <li>{@code A}, rule A's mark (4.2), the value in {@code base} and its
 *   number of empty voids in {@code count}.</li>
 * </ul>
 *
 * @param kind The letter of the kind
 * @param form The formation of a copy, or the reason or label
 * @param birth The birthplace of a copy, or "decided"
 * @param bound The bits of the bound voids of a copy
 * @param tag The tag of a copy, empty when none
 * @param base The value a {@code B} or an {@code A} wraps, or -1
 * @param count The empty voids of an {@code A}
 * @since 0.76.0
 */
record Value(
    char kind, String form, String birth, long bound, String tag, int base, int count
) {

    /**
     * Ctor of a copy.
     *
     * @param form The formation
     * @param born Where it is born, with its tag
     * @param bound The bits of its bound voids
     */
    Value(final String form, final Site born, final long bound) {
        this('O', form, born.place(), bound, born.tag(), -1, 0);
    }

    /**
     * Whether it is a copy made in EO code, not by Java and not shared.
     *
     * @return TRUE if it is
     */
    boolean placed() {
        return !Values.JBORN.equals(this.birth) && !"canon".equals(this.birth);
    }
}
