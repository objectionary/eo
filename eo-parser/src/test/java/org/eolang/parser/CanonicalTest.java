/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.function.UnaryOperator;
import org.cactoos.io.InputOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Test case for {@link Canonical}.
 *
 * @since 0.1
 */
final class CanonicalTest {

    @Test
    @Timeout(120L)
    void buildsALongTupleWithoutOverflowing() throws Exception {
        final int size = 600;
        final StringBuilder source = new StringBuilder(size * 8)
            .append(String.format("[] > app%n  * > @%n"));
        for (int idx = 0; idx < size; idx = idx + 1) {
            source.append(String.format("    1%n"));
        }
        MatcherAssert.assertThat(
            "a tuple of 600 elements must become one layer per element, instead of overflowing Saxon as the layers are built",
            new Canonical().apply(
                new EoSyntax(
                    new InputOf(source.toString()), UnaryOperator.identity()
                ).parsed()
            ).nodes("//o[@base='Φ.tuple']"),
            Matchers.hasSize(size)
        );
    }
}
