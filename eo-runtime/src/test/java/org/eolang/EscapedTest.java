/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link Escaped}.
 *
 * @since 0.73.3
 */
final class EscapedTest {

    @ParameterizedTest
    @CsvSource({
        "0x22, '\\\"'",
        "0x5C, '\\\\'",
        "0x08, '\\b'",
        "0x0C, '\\f'",
        "0x0A, '\\n'",
        "0x0D, '\\r'",
        "0x09, '\\t'",
        "0x07, '\\u0007'",
        "0x0B, '\\u000b'",
        "0x7F, '\\u007f'"
    })
    void spellsSpecialGlyph(final int code, final String expected) {
        MatcherAssert.assertThat(
            String.format("Glyph 0x%02X must be spelled as an escape sequence, but it wasnt", code),
            new Escaped((char) code).get(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void keepsOrdinaryGlyphIntact() {
        MatcherAssert.assertThat(
            "Ordinary glyph must stay verbatim, but it didnt",
            new Escaped('ж').get(),
            Matchers.equalTo("ж")
        );
    }
}
