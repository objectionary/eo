/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link BytesLine}.
 * @since 0.57.0
 */
final class BytesLineTest {

    @Test
    void acceptsBytesOnlyRun() {
        MatcherAssert.assertThat(
            "a dash-separated hex run must be recognised as bytes-only",
            BytesLine.isBytesOnly("CA-FE-BE-BE"),
            Matchers.is(true)
        );
    }

    @Test
    void rejectsNonBytesRun() {
        MatcherAssert.assertThat(
            "a lowercase word must not be recognised as bytes-only",
            BytesLine.isBytesOnly("hello"),
            Matchers.is(false)
        );
    }

    @Test
    void rejectsLowercaseHexDigit() {
        MatcherAssert.assertThat(
            "lowercase hex letters belong to NAME, not to BYTES",
            BytesLine.hex('a'),
            Matchers.is(false)
        );
    }
}
