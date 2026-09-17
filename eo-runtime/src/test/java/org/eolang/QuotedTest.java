/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.nio.charset.StandardCharsets;
import java.util.Optional;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Quoted}.
 *
 * @since 0.73.3
 */
final class QuotedTest {

    @Test
    void quotesPlainText() {
        MatcherAssert.assertThat(
            "Text without special glyphs must be quoted as it is, but it wasnt",
            new Quoted("Привет, мир".getBytes(StandardCharsets.UTF_8)).get(),
            Matchers.equalTo(Optional.of("\"Привет, мир\""))
        );
    }

    @Test
    void escapesQuoteAndBackslash() {
        MatcherAssert.assertThat(
            "Quote and backslash must not break out of the literal, but they did",
            new Quoted("say \"hi\\bye\"".getBytes(StandardCharsets.UTF_8)).get(),
            Matchers.equalTo(Optional.of("\"say \\\"hi\\\\bye\\\"\""))
        );
    }

    @Test
    void escapesDeleteCharacter() {
        MatcherAssert.assertThat(
            "Delete character must be spelled as a unicode escape, but it wasnt",
            new Quoted(String.format("%c", 0x7F).getBytes(StandardCharsets.UTF_8)).get(),
            Matchers.equalTo(Optional.of("\"\\u007f\""))
        );
    }
}
