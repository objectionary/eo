/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link SockaddrIn}.
 *
 * @since 0.40.0
 */
final class SockaddrInTest {

    @ParameterizedTest
    @ValueSource(ints = {0, 16})
    void rejectsPaddingWithUnexpectedSize(final int size) {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new SockaddrIn((short) 2, (short) 0, 0, new byte[size]),
            "SockaddrIn must reject padding that is not eight bytes long"
        );
    }
}
