/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.cactoos.io.InputOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Test cases for parser depth limits.
 *
 * @since 0.71.0
 */
final class EoSyntaxDepthTest {

    @Test
    @Timeout(60L)
    void reportsDeeplyNestedFormationsInsteadOfOverflowing() throws Exception {
        MatcherAssert.assertThat(
            "a source nested deeper than the walk allows must answer a parser error, not take the whole process down",
            new EoSyntax(new InputOf(EoSyntaxDepthTest.nested(Stack.DEEPEST * 2)))
                .parsed()
                .xpath("/object/errors/error[contains(text(),'nested deeper than')]/text()"),
            Matchers.hasSize(1)
        );
    }

    @Test
    @Timeout(60L)
    void reportsDeeplyChainedDispatchesInsteadOfOverflowing() throws Exception {
        MatcherAssert.assertThat(
            "a dispatch chain longer than the walk allows must answer a parser error, not take the whole process down",
            new EoSyntax(new InputOf(EoSyntaxDepthTest.chained(Stack.DEEPEST * 3)))
                .parsed()
                .xpath("/object/errors/error[contains(text(),'nested deeper than')]/text()"),
            Matchers.hasSize(1)
        );
    }

    private static String chained(final int hops) {
        return String.format("+package foo%n%n[] > app%n  x%s > @%n", ".y".repeat(hops));
    }

    private static String nested(final int depth) {
        final String eol = String.format("%n");
        final StringBuilder source = new StringBuilder(depth * 16)
            .append("[] > top").append(eol);
        for (int level = 1; level <= depth; level = level + 1) {
            for (int indent = 0; indent < level; indent = indent + 1) {
                source.append("  ");
            }
            source.append("[] > n").append(level).append(eol);
        }
        return source.toString();
    }
}
