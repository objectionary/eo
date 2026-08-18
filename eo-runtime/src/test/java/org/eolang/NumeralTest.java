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
}
