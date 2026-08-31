/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test for {@link BytesOf}.
 * @since 0.1.0
 */
final class BytesOfTest {

    @Test
    void negatesSeveralTimes() {
        final String text = "abc";
        MatcherAssert.assertThat(
            "Double negation should return the original value, but it didn't",
            new BytesOf(text).not().not(),
            Matchers.equalTo(new BytesOf(text))
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
    void convertsSingleByteToString() {
        MatcherAssert.assertThat(
            "Single byte should render with a trailing dash, but it didn't",
            new BytesOf(new byte[]{(byte) 0xFF}).asString(),
            Matchers.equalTo("FF-")
        );
    }

    @Test
    void checksUnderflowForLong() {
        Assertions.assertThrows(
            ExFailure.class,
            new BytesOf("A")::asNumber,
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
    void shiftsEmptyBytes() {
        MatcherAssert.assertThat(
            "arithmetic shift of empty bytes should preserve them",
            new BytesOf(new byte[0]).sshift(1).take(),
            Matchers.equalTo(new byte[0])
        );
    }

    @Test
    void shiftsIntegerMinValueWithoutOverflow() {
        MatcherAssert.assertThat(
            "shift(Integer.MIN_VALUE) should produce an all-zero result, but it didn't",
            new BytesOf(0xFFFFFFFF).shift(Integer.MIN_VALUE).asNumber(Integer.class),
            Matchers.equalTo(0)
        );
    }

    @Test
    void doesNotSupportNegativeBits() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new BytesOf(Integer.MAX_VALUE).sshift(-1),
            "sshift with negative bits should fail with ExFailure, but it didn't"
        );
    }

    @Test
    void doesNotSupportAndOfDifferentLength() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new BytesOf(new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF}).and(
                new BytesOf(new byte[]{(byte) 0x0F})
            ),
            "'and' of operands with different lengths should fail with ExFailure, but it didn't"
        );
    }

    @Test
    void doesNotSupportOrOfDifferentLength() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new BytesOf(new byte[]{(byte) 0x01, (byte) 0x02}).or(
                new BytesOf(new byte[]{(byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06})
            ),
            "'or' of operands with different lengths should fail with ExFailure, but it didn't"
        );
    }

    @Test
    void doesNotSupportXorOfDifferentLength() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new BytesOf(new byte[]{(byte) 0x01, (byte) 0x02}).xor(
                new BytesOf(new byte[]{(byte) 0x03, (byte) 0x04, (byte) 0x05, (byte) 0x06})
            ),
            "'xor' of operands with different lengths should fail with ExFailure, but it didn't"
        );
    }

    @Test
    void staysEqualToRawBytesOfTheSameContent() {
        MatcherAssert.assertThat(
            "a wrapper around a foreign Bytes must equal the raw bytes of the same content",
            new BytesOf(new BytesOfTest.PlainBytes(new byte[] {(byte) 0x01})),
            Matchers.equalTo(new BytesOf(new byte[] {(byte) 0x01}))
        );
    }

    @Test
    void keepsEqualitySymmetricWithForeignBytes() {
        final Bytes wrapped = new BytesOf(new BytesOfTest.PlainBytes(new byte[] {(byte) 0x01}));
        final Bytes raw = new BytesOf(new byte[] {(byte) 0x01});
        MatcherAssert.assertThat(
            "equality must hold in both directions, not only from the raw side",
            wrapped.equals(raw),
            Matchers.equalTo(raw.equals(wrapped))
        );
    }

    @Test
    void hashesForeignBytesByContent() {
        MatcherAssert.assertThat(
            "equal byte sequences must carry equal hash codes, however each was built",
            new BytesOf(new BytesOfTest.PlainBytes(new byte[] {(byte) 0x01})).hashCode(),
            Matchers.equalTo(new BytesOf(new byte[] {(byte) 0x01}).hashCode())
        );
    }

    @Test
    void collapsesEqualBytesInAHashSet() {
        final Set<Bytes> set = new HashSet<>(0);
        set.add(new BytesOf(new BytesOfTest.PlainBytes(new byte[] {(byte) 0x01})));
        set.add(new BytesOf(new byte[] {(byte) 0x01}));
        MatcherAssert.assertThat(
            "two Bytes of the same content must occupy one slot of a HashSet",
            set,
            Matchers.hasSize(1)
        );
    }

    @Test
    void staysUnequalToBytesOfOtherContent() {
        MatcherAssert.assertThat(
            "bytes of different content must not be equal",
            new BytesOf(new BytesOfTest.PlainBytes(new byte[] {(byte) 0x01})),
            Matchers.not(Matchers.equalTo(new BytesOf(new byte[] {(byte) 0x02})))
        );
    }

    @Test
    void staysUnequalToAnObjectThatIsNotBytes() {
        MatcherAssert.assertThat(
            "an object that is not Bytes must not be equal to Bytes",
            new BytesOf(new byte[] {(byte) 0x01}),
            Matchers.not(Matchers.equalTo("not bytes"))
        );
    }

    /**
     * A legal {@link Bytes} that deliberately inherits the identity-based
     * {@link Object#equals(Object)}, as any implementation of the public
     * interface is free to do.
     * @since 0.1.0
     */
    private static final class PlainBytes implements Bytes {

        /**
         * Data.
         */
        private final byte[] data;

        PlainBytes(final byte[] bytes) {
            this.data = Arrays.copyOf(bytes, bytes.length);
        }

        @Override
        public Bytes not() {
            return new BytesOf(this.data).not();
        }

        @Override
        public Bytes and(final Bytes other) {
            return new BytesOf(this.data).and(other);
        }

        @Override
        public Bytes or(final Bytes other) {
            return new BytesOf(this.data).or(other);
        }

        @Override
        public Bytes xor(final Bytes other) {
            return new BytesOf(this.data).xor(other);
        }

        @Override
        public Bytes shift(final int bits) {
            return new BytesOf(this.data).shift(bits);
        }

        @Override
        public Bytes sshift(final int bits) {
            return new BytesOf(this.data).sshift(bits);
        }

        @Override
        public Double asNumber() {
            return new BytesOf(this.data).asNumber();
        }

        @Override
        public <T extends Number> T asNumber(final Class<T> type) {
            return new BytesOf(this.data).asNumber(type);
        }

        @Override
        public String asString() {
            return new BytesOf(this.data).asString();
        }

        @Override
        public byte[] take() {
            return Arrays.copyOf(this.data, this.data.length);
        }
    }
}
