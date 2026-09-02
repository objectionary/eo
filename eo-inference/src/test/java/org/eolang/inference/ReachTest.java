/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Reach}.
 * @since 0.70.0
 */
final class ReachTest {

    @Test
    void readsANameToItsLastCharacter() {
        MatcherAssert.assertThat(
            "the name next-line runs nine characters, but that isnt what was measured",
            new Reach("  next-line > @").from(2),
            Matchers.equalTo(9)
        );
    }

    @Test
    void keepsAStringWholeToItsClosingQuote() {
        MatcherAssert.assertThat(
            "a quoted star is three characters and not one, but the quote was measured alone",
            new Reach("printf \"*\" x").from(7),
            Matchers.equalTo(3)
        );
    }

    @Test
    void readsPastAnEscapedQuoteInsideAString() {
        MatcherAssert.assertThat(
            "a backslash takes the quote after it, so the string runs on, but it was cut short",
            new Reach("x \"a\\\"b\" y").from(2),
            Matchers.equalTo(6)
        );
    }

    @Test
    void carriesTheDecimalPointOfANumber() {
        MatcherAssert.assertThat(
            "3.14 is four characters of one number, but it was measured shorter",
            new Reach("plus 3.14").from(5),
            Matchers.equalTo(4)
        );
    }

    @Test
    void takesTheSignOfANegativeNumber() {
        MatcherAssert.assertThat(
            "-1 is a number of two characters and not a lone dash, but it measured otherwise",
            new Reach("times -1").from(6),
            Matchers.equalTo(2)
        );
    }

    @Test
    void keepsADottedNameWithItsDot() {
        MatcherAssert.assertThat(
            "the dispatch .as-bytes is nine characters counting its dot, but it wasnt",
            new Reach("first.as-bytes").from(5),
            Matchers.equalTo(9)
        );
    }

    @Test
    void keepsADispatchOfTheCaretWithItsDot() {
        MatcherAssert.assertThat(
            "the dispatch .^ is two characters counting its dot, but it wasnt",
            new Reach("* ^.^").from(3),
            Matchers.equalTo(2)
        );
    }

    @Test
    void measuresNothingPastTheEndOfTheLine() {
        MatcherAssert.assertThat(
            "a column beyond the line cannot reach anything, but it claimed to",
            new Reach("[] > oak").from(40),
            Matchers.equalTo(0)
        );
    }
}
