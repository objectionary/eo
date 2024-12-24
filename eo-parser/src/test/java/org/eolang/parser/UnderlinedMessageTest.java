/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.parser;

import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link UnderlinedMessage}.
 * @since 0.50
 * @checkstyle ParameterNumberCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class UnderlinedMessageTest {

    @ParameterizedTest
    @MethodSource("examples")
    void addsUndeline(final String input, final int from, final int length, final String expected) {
        MatcherAssert.assertThat(
            "We expect the message to be highlighted with underline characters",
            new UnderlinedMessage(input, from, length).formatted(),
            Matchers.equalTo(expected)
        );
    }

    /**
     * Test cases for {@link UnderlinedMessageTest#addsUndeline}.
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
