/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Hex}.
 * @since 0.1
 */
final class HexTest {

    @Test
    void encodesEmptyAsDoubleDash() {
        MatcherAssert.assertThat(
            "an empty byte array must render as `--` per the BYTES token rule",
            new Hex(new byte[0]).asString(),
            Matchers.equalTo("--")
        );
    }

    @Test
    void encodesSingleByteWithTrailingDash() {
        MatcherAssert.assertThat(
            "a single byte must render as `BB-` per the §3.13 single-byte form",
            new Hex(new byte[]{(byte) 0xFF}).asString(),
            Matchers.equalTo("FF-")
        );
    }

    @Test
    void encodesMultiByteJoinedByDashes() {
        MatcherAssert.assertThat(
            "multi-byte input must render as `BB-BB-…` with no trailing dash",
            new Hex(new byte[]{(byte) 0xCA, (byte) 0xFE, (byte) 0xBE}).asString(),
            Matchers.equalTo("CA-FE-BE")
        );
    }

    @Test
    void encodesDoubleAsBytes() {
        MatcherAssert.assertThat(
            "a double must encode as 8 IEEE-754 big-endian bytes",
            new Hex(1.0d).asString(),
            Matchers.equalTo("3F-F0-00-00-00-00-00-00")
        );
    }

    @Test
    void encodesIntegerAsDouble() {
        MatcherAssert.assertThat(
            "an integer value passed via the double ctor must render its IEEE-754 form",
            new Hex(42.0d).asString(),
            Matchers.equalTo("40-45-00-00-00-00-00-00")
        );
    }

    @Test
    void encodesAsciiStringAsBytes() {
        MatcherAssert.assertThat(
            "an ASCII string must encode as its UTF-8 byte sequence",
            new Hex("ABC").asString(),
            Matchers.equalTo("41-42-43")
        );
    }

    @Test
    void encodesUtfString() {
        MatcherAssert.assertThat(
            "a non-ASCII string must encode as its multi-byte UTF-8 sequence",
            new Hex("ä").asString(),
            Matchers.equalTo("C3-A4")
        );
    }

    @Test
    void preservesByteArrayContent() {
        MatcherAssert.assertThat(
            "passing raw bytes must round-trip them unchanged",
            new Hex(new byte[]{0x00, 0x10, 0x20}).asString(),
            Matchers.equalTo("00-10-20")
        );
    }
}
