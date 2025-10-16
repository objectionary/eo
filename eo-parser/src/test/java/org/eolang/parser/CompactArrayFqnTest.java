/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Tests for {@link CompactArrayFqn}.
 *
 * @since 0.57.2
 */
final class CompactArrayFqnTest {

    @ParameterizedTest
    @CsvSource(
        {
            "foo *1,foo",
            "QQ.foo.bar *42,Φ.org.eolang.foo.bar",
            "QQ.nan *52,Φ.org.eolang.nan"
        }
    )
    void buildsFqnForSimpleName(final String compact, final String expected) {
        final String fqn = new CompactArrayFqn(compact).asString();
        MatcherAssert.assertThat(
            String.format(
                "FQN of the compact array: '%s' does not match with expected value: '%s'",
                fqn, expected
            ),
            fqn,
            Matchers.equalTo(expected)
        );
    }
}
