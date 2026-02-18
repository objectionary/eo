/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test for {@link BytesOf}.
 *
 * @since 0.1.0
 */
@SuppressWarnings("PMD.TooManyMethods")
final class BytesOfTest {

    @Test
    void negatesSeveralTimes() {
        MatcherAssert.assertThat(
            "Double negation should return the original value, but it didn't",
            new BytesOf("abc").not().not(),
            Matchers.equalTo(new BytesOf("abc"))
        );
    }

    @Test
    void negatesOnce() {
        MatcherAssert.assertThat(
            "Negation should give the correct value, but it didn't",
            new BytesOf(-128L).not(),
            Matchers.equalTo(new BytesOf(127L))
        );
    }

    @Test
    void checksAnd() {
        final Bytes bytes = new BytesOf(127L);
        MatcherAssert.assertThat(
            "'And' operation with opposite values should give 0, but it didn't",
            bytes.and(bytes.not()),
            Matchers.equalTo(new BytesOf(0L))
        );
    }

    @Test
    void checksOr() {
        final Bytes bytes = new BytesOf(127L);
        MatcherAssert.assertThat(
            "'Or' operation with opposite values should give -1, but it didn't",
            bytes.or(bytes.not()),
            Matchers.equalTo(new BytesOf(-1L))
        );
    }

    @Test
    void checksPositiveInfinity() {
        MatcherAssert.assertThat(
            "Dividing by 0 should give a positive infinity, but it didn't",
            new BytesOf(1.0d / 0.0d).asNumber(),
            Matchers.equalTo(Double.POSITIVE_INFINITY)
        );
    }

    @Test
    void checksNegativeInfinity() {
        MatcherAssert.assertThat(
            "Dividing by 0 should give a negative infinity, but it didn't",
            new BytesOf(-1.0d / 0.0d).asNumber(Double.class),
            Matchers.equalTo(Double.NEGATIVE_INFINITY)
        );
    }

    @Test
    void checksXor() {
        MatcherAssert.assertThat(
            "'Xor' operation should give correct value, but it didn't",
            new BytesOf(512L).xor(new BytesOf(-512L)),
            Matchers.equalTo(new BytesOf(-1024L))
        );
    }

    @Test
    void checksAsNumberLong() {
        MatcherAssert.assertThat(
            "Bytes as long number must be equals to correct number, but it didn't",
            new BytesOf(512L).asNumber(Long.class),
            Matchers.equalTo(512L)
        );
    }

    @Test
    void checksUnderflowForLong() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new BytesOf("A").asNumber(),
            "Converting non-numeric bytes to number should throw, but it didn't"
        );
    }

    @ParameterizedTest
    @CsvSource({
        "0x000000FF,  -8, 0x0000FF00",
        "0x000000FF, -16, 0x00FF0000",
        "0x000000FF, -24, 0xFF000000",
        "0xFF000000,   8, 0x00FF0000",
        "0xFF000000,  16, 0x0000FF00",
        "0xFF000000,  24, 0x000000FF",
        "0x000000FF,   8, 0x00000000"
    })
    void checksShift(final long num, final int bits, final long expected) {
        MatcherAssert.assertThat(
            String.format(
                "%d >> %d should result in %d, but it didn't",
                num, bits, expected
            ),
            new BytesOf(num).shift(bits).asNumber(Long.class),
            Matchers.equalTo(expected)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "0x00000000,   8, 0x00000000",
        "0xFFFFFFFF,   8, 0xFFFFFFFF",
        "0xFF000000,   8, 0xFFFF0000",
        "0xFF000000,  16, 0xFFFFFF00",
        "0xFF000000,  24, 0xFFFFFFFF",
        "0x0000000C,   2, 0x00000003",
        "0xFFFFFFF4,   2, 0xFFFFFFFD",
        "0xFFFFFF80,   3, 0xFFFFFFF0",
        "0xFFFFFC00,   3, 0xFFFFFF80"
    })
    void checksShifts(final long num, final int bits, final long expected) {
        MatcherAssert.assertThat(
            String.format(
                "%d >> %d (arithmetic shift) should result in %d, but it didn't",
                num, bits, expected
            ),
            new BytesOf((int) num).sshift(bits).asNumber(Integer.class),
            Matchers.equalTo((int) expected)
        );
    }

    @Test
    void doesNotSupportRightShift() {
        final Bytes bytes = new BytesOf(Integer.MAX_VALUE);
        Assertions.assertThrows(
            UnsupportedOperationException.class,
            () -> bytes.sshift(-1),
            "Integer.MAX_VALUE << 1 should throw exception, but it didn't"
        );
    }
}
