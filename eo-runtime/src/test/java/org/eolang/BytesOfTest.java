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
 * @since 1.0
 */
final class BytesOfTest {

    @Test
    void negatesSeveralTimes() {
        final String text = "abc";
        final Bytes bytes = new BytesOf(text);
        MatcherAssert.assertThat(
            bytes.not().not(),
            Matchers.equalTo(new BytesOf(text))
        );
    }

    @Test
    void negatesOnce() {
        final Bytes bytes = new BytesOf(-128L);
        MatcherAssert.assertThat(
            bytes.not(),
            Matchers.equalTo(new BytesOf(127L))
        );
    }

    @Test
    void checksAnd() {
        final Bytes bytes = new BytesOf(127L);
        MatcherAssert.assertThat(
            bytes.and(bytes.not()),
            Matchers.equalTo(new BytesOf(0L))
        );
    }

    @Test
    void checksOr() {
        final Bytes bytes = new BytesOf(127L);
        MatcherAssert.assertThat(
            bytes.or(bytes.not()),
            Matchers.equalTo(new BytesOf(-1L))
        );
    }

    @Test
    void checksPositiveInfinity() {
        MatcherAssert.assertThat(
            new BytesOf(1.0d / 0.0d).asNumber(Double.class),
            Matchers.equalTo(Double.POSITIVE_INFINITY)
        );
    }

    @Test
    void checksNegativeInfinity() {
        MatcherAssert.assertThat(
            new BytesOf(-1.0d / 0.0d).asNumber(Double.class),
            Matchers.equalTo(Double.NEGATIVE_INFINITY)
        );
    }

    @Test
    void checksXor() {
        final Bytes bytes = new BytesOf(512L);
        MatcherAssert.assertThat(
            bytes.xor(new BytesOf(-512L)),
            Matchers.equalTo(new BytesOf(-1024L))
        );
    }

    @Test
    void checksAsNumberLong() {
        final Bytes bytes = new BytesOf(512L);
        MatcherAssert.assertThat(
            bytes.asNumber(Long.class),
            Matchers.equalTo(512L)
        );
    }

    @Test
    void checksUnderflowForLong() {
        final Bytes bytes = new BytesOf("A");
        Assertions.assertThrows(
            UnsupportedOperationException.class,
            () -> bytes.asNumber(Long.class)
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
            actual,
            Matchers.equalTo((int) expected)
        );
    }

    @Test
    void doesNotSupportRightShift() {
        final Bytes bytes = new BytesOf(Integer.MAX_VALUE);
        Assertions.assertThrows(
            UnsupportedOperationException.class,
            () -> bytes.sshift(-1)
        );
    }
}
