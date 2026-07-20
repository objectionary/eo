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
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.SYMBOL, 0);
        weights.put(PenaltyKey.SPACE, 0);
        MatcherAssert.assertThat(
            "Five levels of indentation, with one explicit phi, should cost twenty five points",
            new Penalty(
                String.join(
                    System.lineSeparator(),
                    "[] > foo",
                    "  gt. > @",
                    "    42",
                    "    bar.hello 88"
                ),
                weights
            ).points(),
            Matchers.equalTo(25)
        );
    }

    @Test
    void chargesForPhi() {
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.SYMBOL, 0);
        weights.put(PenaltyKey.SPACE, 0);
        MatcherAssert.assertThat(
            "Two explicit phi attributes on one line should cost thirty points",
            new Penalty("@.eq @", weights).points(),
            Matchers.equalTo(30)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "'f (a) (b)', 38",
        "'(a (b (c 1)))', 114"
    })
    void chargesForParentheses(final String code, final int points) {
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.SYMBOL, 0);
        weights.put(PenaltyKey.SPACE, 0);
        MatcherAssert.assertThat(
            String.format(
                "The parentheses in %s should cost %d points, deeper nesting charged more",
                code, points
            ),
            new Penalty(code, weights).points(),
            Matchers.equalTo(points)
        );
    }

    @Test
    void chargesForOverflow() {
        MatcherAssert.assertThat(
            "Each character past the 80th column should cost three points",
            new Penalty(
                String.join("", java.util.Collections.nCopies(85, "x")),
                Collections.singletonMap(PenaltyKey.SYMBOL, 0)
            ).points(),
            Matchers.equalTo(15)
        );
    }

    @Test
    void chargesForSymbols() {
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.INDENT, 0);
        weights.put(PenaltyKey.BRACKET, 0);
        weights.put(PenaltyKey.SPACE, 0);
        MatcherAssert.assertThat(
            "Every symbol in the block should cost one point",
            new Penalty("42.gt (bar.hello 88) > [] > foo", weights).points(),
            Matchers.equalTo(31)
        );
    }

    @Test
    void chargesForApplications() {
        MatcherAssert.assertThat(
            "Four application spaces should cost four squared times seven points",
            new Penalty(
                "foo 4 5 6 7",
                Collections.singletonMap(PenaltyKey.SYMBOL, 0)
            ).points(),
            Matchers.equalTo(112)
        );
    }

    @Test
    void chargesApplicationsSuperLinearly() {
        final Map<PenaltyKey, Integer> weights = new EnumMap<>(PenaltyKey.class);
        weights.put(PenaltyKey.SYMBOL, 0);
        MatcherAssert.assertThat(
            "Doubling the applied arguments should more than double the charge",
            new Penalty("foo a b c d", weights).points(),
            Matchers.greaterThan(2 * new Penalty("foo a b", weights).points())
        );
    }

    @Test
    void treatsThrowingMarkerAsBinding() {
        final Map<PenaltyKey, Integer> weights =
            Collections.singletonMap(PenaltyKey.SYMBOL, 0);
        MatcherAssert.assertThat(
            "The throwing marker --> should bind a name like ++>, not apply arguments",
            new Penalty("foo --> name", weights).points(),
            Matchers.allOf(
                Matchers.equalTo(new Penalty("foo ++> name", weights).points()),
                Matchers.lessThan(new Penalty("foo bar name", weights).points())
            )
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
        final Map<PenaltyKey, Integer> weights =
            Collections.singletonMap(PenaltyKey.SPACE, 0);
        MatcherAssert.assertThat(
            "The flatter rendering should score lower than the deeply nested one",
            new Penalty("nan.plus negative-infinity > x", weights).points(),
            Matchers.lessThan(
                new Penalty(
                    String.join(
                        System.lineSeparator(), "nan.plus > x", "  negative-infinity"
                    ),
                    weights
                ).points()
            )
        );
    }
}
