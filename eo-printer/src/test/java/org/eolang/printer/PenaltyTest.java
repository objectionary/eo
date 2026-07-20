/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link Penalty}.
 * @since 0.57.0
 */
final class PenaltyTest {

    @Test
    void chargesForIndentation() {
        MatcherAssert.assertThat(
            "Five levels of indentation, with one explicit phi, should cost thirty points",
            new Penalty(
                String.join(
                    System.lineSeparator(),
                    "[] > foo",
                    "  gt. > @",
                    "    42",
                    "    bar.hello 88"
                )
            ).points(),
            Matchers.equalTo(30)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "'@.eq @', 30",
        "'foo.if a b c', 50",
        "'if. foo a b c', 0",
        "'foo.iffy a b', 0",
        "'foo.if', 50",
        "'x.if.gt y', 50"
    })
    void chargesForLineAttributes(final String code, final int points) {
        MatcherAssert.assertThat(
            String.format(
                "The line attributes in %s should cost %d points, phi and suffix if charged",
                code, points
            ),
            new Penalty(code).points(),
            Matchers.equalTo(points)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "'42.gt (bar.hello 88) > [] > foo', 7",
        "'(a (b (c 1)))', 42"
    })
    void chargesForParentheses(final String code, final int points) {
        MatcherAssert.assertThat(
            String.format(
                "The parentheses in %s should cost %d points, deeper nesting charged more",
                code, points
            ),
            new Penalty(code).points(),
            Matchers.equalTo(points)
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
    void chargesDefaultsForAbsentKeys() {
        MatcherAssert.assertThat(
            "An empty weights map should behave exactly like the defaults",
            new Penalty("42.gt (bar.hello 88) > [] > foo", Collections.emptyMap()).points(),
            Matchers.equalTo(new Penalty("42.gt (bar.hello 88) > [] > foo").points())
        );
    }

    @Test
    void honoursOverriddenBracketWeight() {
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.BRACKET, 100);
        MatcherAssert.assertThat(
            "A single parenthesis should cost the overridden weight",
            new Penalty("42.gt (bar.hello 88) > [] > foo", weights).points(),
            Matchers.equalTo(100)
        );
    }

    @Test
    void honoursOverriddenStepWeight() {
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.STEP, 4);
        MatcherAssert.assertThat(
            "Two levels of four-space indentation should cost two indents",
            new Penalty(
                String.join(System.lineSeparator(), "[] > foo", "        bar"),
                weights
            ).points(),
            Matchers.equalTo(6)
        );
    }

    @Test
    void honoursOverriddenWidthWeight() {
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.WIDTH, 40);
        MatcherAssert.assertThat(
            "Characters past the overridden 40th column should be charged",
            new Penalty(
                String.join("", java.util.Collections.nCopies(45, "x")), weights
            ).points(),
            Matchers.equalTo(5)
        );
    }

    @Test
    void prefersHorizontalOverDeepVertical() {
        MatcherAssert.assertThat(
            "The flatter rendering should score lower than the deeply nested one",
            new Penalty("nan.plus negative-infinity > x").points(),
            Matchers.lessThan(
                new Penalty(
                    String.join(
                        System.lineSeparator(), "nan.plus > x", "  negative-infinity"
                    )
                ).points()
            )
        );
    }
}
