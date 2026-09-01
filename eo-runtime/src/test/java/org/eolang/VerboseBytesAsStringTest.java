/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.nio.ByteBuffer;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link VerboseBytesAsString}.
 * @since 0.1
 */
final class VerboseBytesAsStringTest {

    @Test
    void representsEightByteValueWithoutUnmatchedParenthesis() {
        MatcherAssert.assertThat(
            "Eight-byte output must not contain an unmatched parenthesis",
            new VerboseBytesAsString(
                ByteBuffer.allocate(Double.BYTES).putDouble(12.345_67d).array()
            ).get(),
            Matchers.equalTo(
                "[0x4028B0FB-A8826AA9] = 12.34567, or \"@(\\ufffd\\ufffd\\ufffd\\ufffdj\\ufffd\""
            )
        );
    }

    @Test
    void groupsHexWithoutTrailingDelimiter() {
        MatcherAssert.assertThat(
            "A hex field ending on an eight-character boundary must not carry a trailing hyphen",
            new VerboseBytesAsString(new byte[]{0, 0, 0, 0, 0, 0, 0, 0}).get(),
            Matchers.equalTo(
                "[0x00000000-00000000] = 0.0, or \"".concat("\\u0000".repeat(8)).concat("\"")
            )
        );
    }

    @Test
    void groupsNineBytesWithSeparatorsBetweenGroupsOnly() {
        MatcherAssert.assertThat(
            "A nine-byte hex field must separate groups without a leading or trailing hyphen",
            new VerboseBytesAsString(
                new byte[]{1, 2, 3, 4, 5, 6, 7, 8, 9}
            ).get(),
            Matchers.containsString("[0x01020304-05060708-09]")
        );
    }

    @ParameterizedTest
    @MethodSource("getTestSources")
    void representsString(final byte[] bytes, final String text) {
        MatcherAssert.assertThat(
            "Bytes must be translated to string correctly",
            new VerboseBytesAsString(bytes).get(),
            Matchers.containsString(text)
        );
    }

    private static Stream<Arguments> getTestSources() {
        return Stream.of(
            Arguments.of(
                ByteBuffer.allocate(Double.BYTES).putDouble(12.345_67d).array(),
                "12.34567"
            ),
            Arguments.of(new byte[]{1}, "[0x01] = true"),
            Arguments.of(new byte[]{0}, "[0x00] = false"),
            Arguments.of(new byte[]{2}, "[0x02] = false"),
            Arguments.of(new byte[]{}, "[<no bytes>]"),
            Arguments.of(new byte[]{12}, "[0x0C] = false"),
            Arguments.of(
                new byte[]{0x61, 0x22, 0x62, 0x5C, 0x63, 0x7F},
                "[0x6122625C-637F] = \"a\\\"b\\\\c\\u007f\""
            ),
            Arguments.of(
                new byte[]{10, 11, 12, 13, 14, 15, 16, 17, -18, -19, -20, -21, 22},
                "[0x0A0B0C0D-0E0F1011-EEEDECEB-16] = \"\\u000a\\u000b\\u000c\\u000d\\u000e\\u000f\\u0010\\u0011\\ufffd\\ufffd\\ufffd\\ufffd\\u0016\""
            )
        );
    }
}
