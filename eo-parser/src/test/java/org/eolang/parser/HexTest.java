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
 *
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
    void preservesByteArrayContent() {
        MatcherAssert.assertThat(
            "passing raw bytes must round-trip them unchanged",
            new Hex(new byte[]{0x00, 0x10, 0x20}).asString(),
            Matchers.equalTo("00-10-20")
        );
    }

    @Test
    void keepsOwnCopyOfConstructorArgument() {
        final byte[] raw = {0x00, 0x10, 0x20};
        final Hex hex = new Hex(raw);
        raw[0] = (byte) 0xFF;
        MatcherAssert.assertThat(
            "mutating the caller's array after construction must not change the rendered bytes",
            hex.asString(),
            Matchers.equalTo("00-10-20")
        );
    }

    @Test
    void ignoresMutationOfSingleByteArrayAfterConstruction() {
        final byte[] raw = {0x7A};
        final Hex hex = new Hex(raw);
        raw[0] = 0x00;
        MatcherAssert.assertThat(
            "mutating a single-byte array after construction must not change the rendered byte",
            hex.asString(),
            Matchers.equalTo("7A-")
        );
    }

    @Test
    void ignoresZeroingOfArrayAfterConstruction() {
        final byte[] raw = {0x01, 0x02, 0x03, 0x04};
        final Hex hex = new Hex(raw);
        for (int idx = 0; idx < raw.length; idx = idx + 1) {
            raw[idx] = 0x00;
        }
        MatcherAssert.assertThat(
            "zeroing the caller's array after construction must not change the rendered bytes",
            hex.asString(),
            Matchers.equalTo("01-02-03-04")
        );
    }

    @Test
    void keepsEarlierInstanceUnaffectedByReusingTheSameArray() {
        final byte[] shared = {0x05, 0x06};
        final Hex first = new Hex(shared);
        shared[0] = 0x09;
        MatcherAssert.assertThat(
            "reusing the caller's array for a later Hex must not touch an earlier one",
            first.asString(),
            Matchers.equalTo("05-06")
        );
    }
}
