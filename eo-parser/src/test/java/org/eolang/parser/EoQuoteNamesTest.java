/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Names containing quotes inside grouped expressions.
 *
 * @since 0.1
 */
final class EoQuoteNamesTest {

    @ParameterizedTest
    @ValueSource(
        strings = {
            "f (g a\"b) > x",
            "* a\"b > x",
            "f (a\"b > []) > x",
            "f (g \"a(b)\") > x"
        }
    )
    void distinguishesNamesFromStringLiterals(final String expression) throws Exception {
        MatcherAssert.assertThat(
            "a quote inside a name must not start a string, while real strings keep their parentheses",
            new EoSyntax(
                String.format("+package foo%n%n[] > foo%n  %s%n  x > @%n", expression)
            ).parsed().xpath("/object/errors/error/text()"),
            Matchers.empty()
        );
    }

    @Test
    void reportsRedundantParentheses() throws Exception {
        MatcherAssert.assertThat(
            "a grouped name must report redundant parentheses instead of an unterminated string",
            new EoSyntax(
                String.format("+package foo%n%n[] > foo%n  f (a\"b) > x%n  x > @%n")
            ).parsed().xpath("/object/errors/error/text()"),
            Matchers.hasItem(Matchers.containsString("redundant parentheses"))
        );
    }
}
