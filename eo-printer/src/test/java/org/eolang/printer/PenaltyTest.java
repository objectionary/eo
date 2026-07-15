/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Penalty}.
 * @since 0.57.0
 */
final class PenaltyTest {

    @Test
    void chargesForIndentation() {
        MatcherAssert.assertThat(
            "Five levels of indentation should cost fifteen points",
            new Penalty(
                String.join(
                    "\n",
                    "[] > foo",
                    "  gt. > @",
                    "    42",
                    "    bar.hello 88"
                )
            ).points(),
            Matchers.equalTo(15)
        );
    }

    @Test
    void chargesForParenthesis() {
        MatcherAssert.assertThat(
            "A single opening parenthesis on one line should cost seven points",
            new Penalty("42.gt (bar.hello 88) > [] > foo").points(),
            Matchers.equalTo(7)
        );
    }

    @Test
    void chargesForOverflow() {
        MatcherAssert.assertThat(
            "Each character past the 80th column should cost one point",
            new Penalty(String.join("", java.util.Collections.nCopies(85, "x"))).points(),
            Matchers.equalTo(5)
        );
    }

    @Test
    void chargesNothingForEmptyCode() {
        MatcherAssert.assertThat(
            "Empty code should have zero penalty",
            new Penalty("").points(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void prefersHorizontalOverDeepVertical() {
        MatcherAssert.assertThat(
            "The flatter rendering should score lower than the deeply nested one",
            new Penalty("nan.plus negative-infinity > x").points(),
            Matchers.lessThan(
                new Penalty(
                    String.join("\n", "nan.plus > x", "  negative-infinity")
                ).points()
            )
        );
    }
}
