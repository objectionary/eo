/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhDefault} numeral rendering.
 * @since 0.1
 */
final class PhDefaultNumeralTest {

    @Test
    void printsPositiveIntegralNumberOutsideLongRange() {
        final double number = Math.scalb(1.0d, 63);
        MatcherAssert.assertThat(
            "Positive integral double outside long range must retain its value in φ-term",
            PhDefault.numeral(number),
            Matchers.equalTo(Double.toString(number))
        );
    }

    @Test
    void printsNegativeIntegralNumberOutsideLongRange() {
        final double number = -Math.scalb(1.0d, 64);
        MatcherAssert.assertThat(
            "Negative integral double outside long range must retain its value in φ-term",
            PhDefault.numeral(number),
            Matchers.equalTo(Double.toString(number))
        );
    }
}
