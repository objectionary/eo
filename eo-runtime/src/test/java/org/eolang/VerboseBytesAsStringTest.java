/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.nio.ByteBuffer;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link VerboseBytesAsString}.
 *
 * @since 0.1
 */
final class VerboseBytesAsStringTest {

    @ParameterizedTest
    @MethodSource("getTestSources")
    void representsString(final byte[] bytes, final String text) {
        MatcherAssert.assertThat(
            "Bytes must be translated to string correctly",
            new VerboseBytesAsString(bytes).get(),
            Matchers.containsString(text)
        );
    }

    /**
     * Static method providing sources for parameterized test.
     * @return Stream of sources.
     */
    private static Stream<Arguments> getTestSources() {
        return Stream.of(
            Arguments.of(
                ByteBuffer.allocate(Double.BYTES).putDouble(12.345_67D).array(),
                "12.34567"
            ),
            Arguments.of(new byte[]{1}, "[0x01] = true"),
            Arguments.of(new byte[]{0}, "[0x00] = false"),
            Arguments.of(new byte[]{}, "[<no bytes>]"),
            Arguments.of(new byte[]{12}, "[0x0C] = true"),
            Arguments.of(
                new byte[]{10, 11, 12, 13, 14, 15, 16, 17, -18, -19, -20, -21, 22},
                "[0x0A0B0C0D-0E0F1011-EEEDECEB-16] = \"\\u000a\\u000b\\u000c\\u000d\\u000e\\u000f\\u0010\\u0011\\ufffd\\ufffd\\ufffd\\ufffd\\u0016\""
            )
        );
    }
}
