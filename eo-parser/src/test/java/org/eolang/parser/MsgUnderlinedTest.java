/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link MsgUnderlined}.
 *
 * @since 0.50
 */
final class MsgUnderlinedTest {

    @Test
    void underlinesWholeLineWhenFromIsNegative() {
        MatcherAssert.assertThat(
            "a negative from must underline every character of the origin",
            new MsgUnderlined("hello", -1, 3).formatted(),
            Matchers.equalTo(String.format("hello%n^^^^^"))
        );
    }

    @Test
    void clampsCaretRunToLineLengthRemainingAfterFrom() {
        MatcherAssert.assertThat(
            "a length reaching past the line end must be clamped to what remains from position",
            new MsgUnderlined("0123456789", 8, 5).formatted(),
            Matchers.equalTo(String.format("0123456789%n        ^^"))
        );
    }

    @Test
    void leavesUnderlineEmptyWhenFromReachesLineEnd() {
        MatcherAssert.assertThat(
            "a from at the line length must draw no caret since no position is left to underline",
            new MsgUnderlined("abc", 3, 2).formatted(),
            Matchers.equalTo(String.format("abc%n"))
        );
    }
}
