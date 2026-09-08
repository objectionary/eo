/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Numeral}.
 *
 * @since 0.73.3
 */
final class NumeralTest {

    @Test
    void printsPositiveIntegralNumberOutsideLongRange() {
        final double number = Math.scalb(1.0d, 63);
        MatcherAssert.assertThat(
            "Positive integral double outside long range must retain its value in φ-term",
            new Numeral(number).get(),
            Matchers.equalTo(Double.toString(number))
        );
    }

    @Test
    void printsNegativeIntegralNumberOutsideLongRange() {
        final double number = -Math.scalb(1.0d, 64);
        MatcherAssert.assertThat(
            "Negative integral double outside long range must retain its value in φ-term",
            new Numeral(number).get(),
            Matchers.equalTo(Double.toString(number))
        );
    }

    @Test
    void printsNegativeZeroWithItsSign() {
        MatcherAssert.assertThat(
            "Negative zero must keep its sign in φ-term",
            new Numeral(-0.0d).get(),
            Matchers.equalTo("-0.0")
        );
    }

    @Test
    void printsPositiveZeroWithoutSign() {
        MatcherAssert.assertThat(
            "Positive zero must print without a sign in φ-term",
            new Numeral(0.0d).get(),
            Matchers.equalTo("0")
        );
    }

    @Test
    void printsNegativeZeroComputedByDivision() {
        MatcherAssert.assertThat(
            "Negative zero produced by division must keep its sign in φ-term",
            new Numeral(-1.0d / Double.POSITIVE_INFINITY).get(),
            Matchers.equalTo("-0.0")
        );
    }

    @Test
    void printsNegativeZeroWidenedFromFloat() {
        MatcherAssert.assertThat(
            "Negative zero widened from float must keep its sign in φ-term",
            new Numeral(-0.0f).get(),
            Matchers.equalTo("-0.0")
        );
    }

    @Test
    void printsNotANumberTheWayEoSpellsIt() {
        MatcherAssert.assertThat(
            "NaN must print as an EO number in φ-term",
            new Numeral(Double.NaN).get(),
            Matchers.equalTo("nan")
        );
    }

    @Test
    void printsPositiveInfinityTheWayEoSpellsIt() {
        MatcherAssert.assertThat(
            "Positive infinity must print as an EO number in φ-term",
            new Numeral(Double.POSITIVE_INFINITY).get(),
            Matchers.equalTo("pinf")
        );
    }

    @Test
    void printsNegativeInfinityTheWayEoSpellsIt() {
        MatcherAssert.assertThat(
            "Negative infinity must print as an EO number in φ-term",
            new Numeral(Double.NEGATIVE_INFINITY).get(),
            Matchers.equalTo("ninf")
        );
    }

    @Test
    void printsRegularNegativeIntegralNumberWithoutFraction() {
        MatcherAssert.assertThat(
            "Ordinary negative integral double must print without a fraction in φ-term",
            new Numeral(-5.0d).get(),
            Matchers.equalTo("-5")
        );
    }
}
