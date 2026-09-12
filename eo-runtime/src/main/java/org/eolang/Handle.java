/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Transform {@link Expect} to the Long of a native handle.
 *
 * <p>A socket descriptor is a handle the operating system gave out, so a
 * number that is not a whole one names no handle at all. Letting
 * {@code Double.longValue()} do the conversion would turn {@code 2.5} into
 * the handle {@code 2}, which belongs to somebody else, and {@code nan} into
 * zero, so the numbers are refused here instead (#8143). The range is the one
 * a double still counts in whole steps: past {@code 2^53} the neighbours of a
 * number are more than one apart, so a handle read from there is not the one
 * that was written.</p>
 *
 * <p>Public because the Win32 adapters, which live in another package, map EO
 * numbers onto native handles, the way {@link Int} maps them onto C
 * {@code int} parameters.</p>
 *
 * @since 0.76
 */
public final class Handle {

    /**
     * Expect.
     */
    private final Expect<Phi> expect;

    /**
     * Ctor.
     * @param subject What the number is, for the failure message
     * @param phi The object holding the number
     */
    public Handle(final String subject, final Phi phi) {
        this(new Expect<>(subject, () -> phi));
    }

    /**
     * Ctor.
     * @param expect Expect
     */
    public Handle(final Expect<Phi> expect) {
        this.expect = expect;
    }

    /**
     * Return it.
     * @return The handle
     */
    public Long it() {
        return this.expect
            .that(phi -> new Dataized(phi).asNumber())
            .otherwise("must be a number")
            .must(number -> !number.isNaN() && !number.isInfinite())
            .otherwise("must be finite")
            .must(number -> number % 1 == 0)
            .otherwise("must be an integer")
            .must(number -> Math.abs(number) <= 9_007_199_254_740_992.0d)
            .otherwise("must not exceed 2^53 in magnitude")
            .that(Double::longValue)
            .it();
    }
}
