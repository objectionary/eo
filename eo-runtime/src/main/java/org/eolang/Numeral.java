/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.function.Supplier;

/**
 * A number value rendered as a φ-term.
 *
 * <p>A whole value inside the {@code long} range prints without a
 * fraction, so {@code 42} rather than {@code 42.0}. Everything else —
 * fractional, infinite, or beyond {@code long} — keeps its full
 * {@code double} spelling: casting an out-of-range value to {@code long}
 * would saturate and denote a different number.</p>
 *
 * @since 0.73.3
 */
final class Numeral implements Supplier<String> {

    /**
     * The number.
     */
    private final double value;

    /**
     * Ctor.
     * @param num The number
     */
    Numeral(final double num) {
        this.value = num;
    }

    @Override
    public String get() {
        final String txt;
        if (
            this.value == Math.floor(this.value)
                && !Double.isInfinite(this.value)
                && Long.MIN_VALUE <= this.value
                && this.value < Long.MAX_VALUE
        ) {
            txt = Long.toString((long) this.value);
        } else {
            txt = Double.toString(this.value);
        }
        return txt;
    }
}
