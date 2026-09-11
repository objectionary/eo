/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Hex}.
 *
 * @since 0.77.0
 */
final class HexTest {

    @Test
    void spellsNumberAsEightDashedBytes() {
        MatcherAssert.assertThat(
            "the number must be spelled as its IEEE bytes, but it wasnt",
            new Hex(3.0d).text(),
            Matchers.equalTo("40-08-00-00-00-00-00-00")
        );
    }

    @Test
    void readsNumberBackFromDashedBytes() {
        MatcherAssert.assertThat(
            "the dashed bytes must read back as the number, but they didnt",
            new Hex("40-45-00-00-00-00-00-00").number(),
            Matchers.equalTo(42.0d)
        );
    }

    @Test
    void spellsSingleByteWithTrailingDash() {
        MatcherAssert.assertThat(
            "one byte must end with a dash, but it didnt",
            new Hex(true).text(),
            Matchers.equalTo("FF-")
        );
    }

    @Test
    void spellsEmptyBytesAsTwoDashes() {
        MatcherAssert.assertThat(
            "no bytes must be spelled as two dashes, but they werent",
            new Hex("--").text(),
            Matchers.equalTo("--")
        );
    }

    @Test
    void refusesNumberOfWrongLength() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Hex("01-02").number(),
            "two bytes cannot be a number, but they were"
        );
    }
}
