/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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
        final String text = "abc";
        final Bytes bytes = new BytesOf(text);
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            bytes.not().not(),
            Matchers.equalTo(new BytesOf(text))
        );
    }

    @Test
    void negatesOnce() {
        final Bytes bytes = new BytesOf(-128L);
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            bytes.not(),
            Matchers.equalTo(new BytesOf(127L))
        );
    }

    @Test
    void checksAnd() {
        final Bytes bytes = new BytesOf(127L);
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            bytes.and(bytes.not()),
            Matchers.equalTo(new BytesOf(0L))
        );
    }

    @Test
    void checksOr() {
        final Bytes bytes = new BytesOf(127L);
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            bytes.or(bytes.not()),
            Matchers.equalTo(new BytesOf(-1L))
        );
    }

    @Test
    void checksPositiveInfinity() {
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            new BytesOf(1.0d / 0.0d).asNumber(),
            Matchers.equalTo(Double.POSITIVE_INFINITY)
        );
    }

    @Test
    void checksNegativeInfinity() {
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            new BytesOf(-1.0d / 0.0d).asNumber(Double.class),
            Matchers.equalTo(Double.NEGATIVE_INFINITY)
        );
    }

    @Test
    void checksXor() {
        final Bytes bytes = new BytesOf(512L);
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            bytes.xor(new BytesOf(-512L)),
            Matchers.equalTo(new BytesOf(-1024L))
        );
    }

    @Test
    void checksAsNumberLong() {
        final Bytes bytes = new BytesOf(512L);
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            bytes.asNumber(Long.class),
            Matchers.equalTo(512L)
        );
    }

    @Test
    void checksUnderflowForLong() {
        final Bytes bytes = new BytesOf("A");
        Assertions.assertThrows(
            ExFailure.class,
            bytes::asNumber,
            PhCompositeTest.TO_ADD_MESSAGE
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
        final Bytes bytes = new BytesOf(num);
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            bytes.shift(bits).asNumber(Long.class),
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
        final Bytes bytes = new BytesOf((int) num);
        final int actual = bytes.sshift(bits).asNumber(Integer.class);
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            actual,
            Matchers.equalTo((int) expected)
        );
    }

    @Test
    void doesNotSupportRightShift() {
        final Bytes bytes = new BytesOf(Integer.MAX_VALUE);
        Assertions.assertThrows(
            UnsupportedOperationException.class,
            () -> bytes.sshift(-1),
            PhCompositeTest.TO_ADD_MESSAGE
        );
    }
}
