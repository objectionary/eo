/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Millis}.
 *
 * @since 0.73.4
 */
final class MillisTest {

    @Test
    void roundsSubMillisecondSpanUpToOne() {
        MatcherAssert.assertThat(
            "a span of a single nanosecond is not printed as one millisecond",
            new Millis(1L).asString(),
            Matchers.equalTo("1")
        );
    }

    @Test
    void roundsPartialMillisecondUp() {
        MatcherAssert.assertThat(
            "a span of two and a half milliseconds is not printed as three",
            new Millis(2_500_000L).asString(),
            Matchers.equalTo("3")
        );
    }

    @Test
    void keepsWholeMillisecondsIntact() {
        MatcherAssert.assertThat(
            "a span of exactly seven milliseconds is not printed as seven",
            new Millis(7_000_000L).asString(),
            Matchers.equalTo("7")
        );
    }
}
