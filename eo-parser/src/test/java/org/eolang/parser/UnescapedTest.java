/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.nio.charset.StandardCharsets;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Unescaped}.
 *
 * @since 0.1
 */
final class UnescapedTest {

    @Test
    void decodesLiteralWithoutEscapes() {
        MatcherAssert.assertThat(
            "a literal carrying no escape must decode to its own characters",
            new String(new Unescaped("привет", 7, 3).bytes(), StandardCharsets.UTF_8),
            Matchers.equalTo("привет")
        );
    }

    @Test
    void carriesOffendingCharactersOfUnknownEscape() {
        MatcherAssert.assertThat(
            "an unknown escape must be reported with the characters that caused it",
            Assertions.assertThrows(
                ParseError.class,
                () -> new Unescaped("r\\c", 7, 3).bytes()
            ).getMessage(),
            Matchers.equalTo("unrecognised escape sequence '\\c'")
        );
    }

    @Test
    void reportsFailureAtTheLiteralPosition() {
        MatcherAssert.assertThat(
            "a failing escape must be reported at the line the literal was given",
            Assertions.assertThrows(
                ParseError.class,
                () -> new Unescaped("\\q", 7, 3).bytes()
            ).line(),
            Matchers.equalTo(7)
        );
    }
}
