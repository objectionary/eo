/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Handle}.
 * @since 0.76
 */
final class HandleTest {

    @Test
    void takesAWholeNumber() {
        MatcherAssert.assertThat(
            "a whole number must be the handle it names, but it wasnt",
            new Handle("the socket", new Data.ToPhi(42.0d)).it(),
            Matchers.equalTo(42L)
        );
    }

    @Test
    void refusesAFraction() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Handle("the socket", new Data.ToPhi(2.5d)).it(),
            "a fraction names no handle and must be refused, but it wasnt"
        );
    }

    @Test
    void refusesNotANumber() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Handle("the socket", new Data.ToPhi(Double.NaN)).it(),
            "nan names no handle and must be refused, but it wasnt"
        );
    }

    @Test
    void refusesInfinity() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Handle("the socket", new Data.ToPhi(Double.POSITIVE_INFINITY)).it(),
            "infinity names no handle and must be refused, but it wasnt"
        );
    }

    @Test
    void refusesWhatADoubleNoLongerCounts() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Handle("the socket", new Data.ToPhi(1.0e18d)).it(),
            "a magnitude past 2^53 must be refused, but it wasnt"
        );
    }
}
