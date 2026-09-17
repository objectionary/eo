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
 * @since 0.77.0
 */
final class NumberedTest {

    @Test
    void rendersEightBytesAsLiteral() {
        MatcherAssert.assertThat(
            "Eight bytes must render as the number they spell, but they didnt",
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
    void refusesTooFewBytes() {
        MatcherAssert.assertThat(
            "One byte spells no number and must come back empty, but it didnt",
            new Numbered(new byte[] {(byte) 0x01}).get().isPresent(),
            Matchers.is(false)
        );
    }

    @Test
    void refusesNoBytesAtAll() {
        MatcherAssert.assertThat(
            "An empty payload spells no number and must come back empty, but it didnt",
            new Numbered(new byte[0]).get().isPresent(),
            Matchers.is(false)
        );
    }

    @Test
    void refusesTooManyBytes() {
        MatcherAssert.assertThat(
            "Nine bytes spell no number and must come back empty, but they didnt",
            new Numbered(new byte[9]).get().isPresent(),
            Matchers.is(false)
        );
    }
}
