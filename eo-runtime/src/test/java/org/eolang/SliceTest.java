/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.security.SecureRandom;
import java.util.Arrays;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Slice}.
 *
 * @since 0.64.0
 */
final class SliceTest {

    @Test
    void cutsTheBytesOfItsRange() {
        final SecureRandom random = new SecureRandom();
        final byte[] bytes = new byte[random.nextInt(29) + 7];
        random.nextBytes(bytes);
        final int start = random.nextInt(bytes.length);
        final int len = random.nextInt(bytes.length - start + 1);
        MatcherAssert.assertThat(
            "a slice inside the bytes must hold the bytes of its range, but it doesnt",
            new Slice(bytes, start, len).delta(),
            Matchers.equalTo(Arrays.copyOfRange(bytes, start, start + len))
        );
    }

    @Test
    void failsWithTheMessageOfTheAtomPastTheEnd() {
        final int size = new SecureRandom().nextInt(17) + 3;
        MatcherAssert.assertThat(
            "a slice past the end must fail the way the atom does, but it doesnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Slice(new byte[size], 2, size).delta(),
                "a slice past the end doesnt fail"
            ).getMessage(),
            Matchers.equalTo(
                String.format(
                    "cannot slice '%d' bytes from offset '2' of bytes of size %d", size, size
                )
            )
        );
    }

    @Test
    void failsOnAStartThatIsNoInteger() {
        MatcherAssert.assertThat(
            "a slice from a fractional offset must fail the way the atom does, but it doesnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Slice(new byte[] {(byte) 0x1C, (byte) 0xA7, (byte) 0x3E}, 1.25, 1)
                    .delta(),
                "a slice from a fractional offset doesnt fail"
            ).getMessage(),
            Matchers.equalTo("the 'start' attribute (1.25) must be an integer")
        );
    }

    @Test
    void failsOnANegativeLength() {
        MatcherAssert.assertThat(
            "a slice of a negative length must fail the way the atom does, but it doesnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Slice(new byte[] {(byte) 0x5D, (byte) 0x02, (byte) 0xF9}, 0, -3)
                    .delta(),
                "a slice of a negative length doesnt fail"
            ).getMessage(),
            Matchers.equalTo("the 'len' attribute (-3) must be greater or equal to zero")
        );
    }
}
