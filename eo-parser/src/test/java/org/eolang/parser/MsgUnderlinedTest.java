/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link MsgUnderlined}.
 * @since 0.50
 * @checkstyle ParameterNumberCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class MsgUnderlinedTest {

    @ParameterizedTest
    @MethodSource("examples")
    void addsUnderline(
        final String input, final int from, final int length, final String expected) {
        MatcherAssert.assertThat(
            "We expect the message to be highlighted with underline characters",
            new MsgUnderlined(input, from, length).formatted(),
            Matchers.equalTo(expected)
        );
    }

    /**
     * Test cases for {@link MsgUnderlinedTest#addsUnderline}.
     * ANTLR {@link  org.antlr.v4.runtime.BaseErrorListener} returns strange line numbers
     * and positions like -1. Here I hide this problem intentionally to make all the rest
     * tests pass.
     * @return Test cases.
     */
    private static Stream<Arguments> examples() {
        final String issue = "Problem is here";
        return Stream.of(
            Arguments.of(issue, 0, 7, "Problem is here\n^^^^^^^"),
            Arguments.of(issue, 8, 2, "Problem is here\n        ^^"),
            Arguments.of(issue, 0, 0, "Problem is here\n"),
            Arguments.of(issue, 0, 1, "Problem is here\n^"),
            Arguments.of(issue, 14, 1, "Problem is here\n              ^"),
            Arguments.of(issue, 0, 15, "Problem is here\n^^^^^^^^^^^^^^^"),
            Arguments.of(issue, -1, 0, "Problem is here\n"),
            Arguments.of(issue, 0, -1, "Problem is here\n"),
            Arguments.of(issue, 0, 100, "Problem is here\n^^^^^^^^^^^^^^^"),
            Arguments.of(issue, 100, 0, "Problem is here\n"),
            Arguments.of(issue, 100, 100, "Problem is here\n"),
            Arguments.of("", 1, 10, "\n")
        );
    }
}
