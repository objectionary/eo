/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link ChNarrow}.
 *
 * @since 0.28.11
 */
final class ChNarrowTest {

    @ParameterizedTest
    @CsvSource({
        "1234567, 1234567",
        "12345678, 1234567",
        "123456789, 1234567",
        "1, 1"
    })
    void cutsHashCorrectly(final String input, final String output) {
        MatcherAssert.assertThat(
            String.format(
                "The hash of %s was calculated incorrectly",
                input
            ),
            new ChNarrow(
                new CommitHash.ChConstant(input)
            ).value(),
            Matchers.equalTo(output)
        );
    }

    @Test
    void throwsExceptionIfEmpty() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new ChNarrow(new CommitHash.ChConstant("")).value(),
            CatalogsTest.TO_ADD_MESSAGE
        );
    }
}
