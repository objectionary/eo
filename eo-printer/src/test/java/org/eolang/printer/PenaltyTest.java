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
 *
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
    void chargesNothingForBracketInsideStringLiteral() {
        MatcherAssert.assertThat(
            "Code inside a string literal must cost what plain text costs, but it didnt",
            new Penalty("\"(((\"").points(),
            Matchers.equalTo(new Penalty("\"xxx\"").points())
        );
    }

    @Test
    void chargesNothingForPhiInsideStringLiteral() {
        MatcherAssert.assertThat(
            "Code inside a string literal must cost what plain text costs, but it didnt",
            new Penalty("\"@@@\"").points(),
            Matchers.equalTo(new Penalty("\"xxx\"").points())
        );
    }

    @Test
    void chargesNothingForIfInsideStringLiteral() {
        MatcherAssert.assertThat(
            "Code inside a string literal must cost what plain text costs, but it didnt",
            new Penalty("\".if .if\"").points(),
            Matchers.equalTo(new Penalty("\"xxx xxx\"").points())
        );
    }

    @Test
    void chargesForBracketOutsideStringLiteral() {
        MatcherAssert.assertThat(
            "A bracket next to a string literal must still cost more than plain text, but it didnt",
            new Penalty("(\"(\") x").points(),
            Matchers.greaterThan(new Penalty("x\"x\"x x").points())
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
