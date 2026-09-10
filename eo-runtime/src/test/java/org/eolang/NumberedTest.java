/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Optional;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Numbered}.
 *
 * @since 0.75.0
 */
final class NumberedTest {

    @Test
    void rendersEightBytesAsANumber() {
        MatcherAssert.assertThat(
            "eight bytes must render as the number they denote, but they didnt",
            new Numbered(
                new byte[] {
                    (byte) 0x40, (byte) 0x45, (byte) 0x00, (byte) 0x00,
                    (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                }
            ).get(),
            Matchers.equalTo(Optional.of("42"))
        );
    }

    @Test
    void refusesBytesOfAnotherWidth() {
        MatcherAssert.assertThat(
            "bytes that are not eight wide must be refused, so the renderer can fall back",
            new Numbered(new byte[] {(byte) 0x01}).get(),
            Matchers.equalTo(Optional.empty())
        );
    }
}
