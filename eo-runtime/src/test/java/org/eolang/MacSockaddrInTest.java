/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.sun.jna.Structure;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link MacSockaddrIn}.
 * @since 0.74.0
 */
final class MacSockaddrInTest {

    @Test
    void opensWithALengthAndAFamily() {
        final Structure struct = new MacSockaddrIn((short) 2, (short) 0, 0, new byte[8]);
        struct.write();
        MatcherAssert.assertThat(
            "the length must come first there and the family second, but they didnt",
            struct.getPointer().getByteArray(0, 2),
            Matchers.equalTo(new byte[] {(byte) 16, (byte) 2})
        );
    }

    @Test
    void refusesPaddingOfAWrongLength() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new MacSockaddrIn((short) 2, (short) 0, 0, new byte[0]),
            "a padding that is not eight bytes long must be refused, but it wasnt"
        );
    }

    @Test
    void keepsTheLayoutOfSixteenBytes() {
        MatcherAssert.assertThat(
            "the structure must stay as long as a sockaddr_in is, but it didnt",
            new MacSockaddrIn((short) 2, (short) 0, 0, new byte[8]).size(),
            Matchers.equalTo(16)
        );
    }
}
