/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Budget}.
 *
 * @since 0.76.0
 */
final class BudgetTest {

    @Test
    void spendsItselfWhenTheSecondsArePast() {
        MatcherAssert.assertThat(
            "a budget of three seconds must be spent five seconds later, but it isnt",
            new Budget(3L, System.currentTimeMillis() - 5_000L).spent(),
            Matchers.is(true)
        );
    }

    @Test
    void keepsItselfWhileTheSecondsRemain() {
        MatcherAssert.assertThat(
            "a budget of seven seconds cannot be spent two seconds later, but it is",
            new Budget(7L, System.currentTimeMillis() - 2_000L).spent(),
            Matchers.is(false)
        );
    }

    @Test
    void ignoresTheClockWithoutSeconds() {
        MatcherAssert.assertThat(
            "a budget of zero seconds cannot be spent however long it waits, but it is",
            new Budget(0L, System.currentTimeMillis() - 900_000L).spent(),
            Matchers.is(false)
        );
    }

    @Test
    void countsFromTheMomentItIsMade() {
        MatcherAssert.assertThat(
            "a fresh budget of one second cannot be spent right away, but it is",
            new Budget(1L).spent(),
            Matchers.is(false)
        );
    }
}
