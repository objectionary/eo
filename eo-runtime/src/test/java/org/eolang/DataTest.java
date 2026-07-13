/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link Data}.
 * @since 0.1
 */
final class DataTest {

    @Test
    void printsWholeNumberValueAsTerm() {
        MatcherAssert.assertThat(
            "Whole number must render as an integer φ-term, but it didnt",
            new Data.ToPhi(42L).φTerm(),
            Matchers.equalTo("42")
        );
    }

    @Test
    void failsWhenObjectTypeIsUnknown() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Data.ToPhi(new Object()),
            "converting an unsupported Java type was expected to fail with ExFailure"
        );
    }

    @Test
    void printsFractionalNumberValueAsTerm() {
        MatcherAssert.assertThat(
            "Fractional number must render with its decimals in φ-term, but it didnt",
            new Data.ToPhi(2.5d).φTerm(),
            Matchers.equalTo("2.5")
        );
    }

    @Test
    void printsStringValueAsTerm() {
        MatcherAssert.assertThat(
            "String must render as a quoted value in φ-term, but it didnt",
            new Data.ToPhi("hello").φTerm(),
            Matchers.equalTo("\"hello\"")
        );
    }

    @Test
    void distinguishesDifferentNumbersInTerm() {
        MatcherAssert.assertThat(
            "Different numbers must produce different φ-terms, but they didnt",
            new Data.ToPhi(5L).φTerm(),
            Matchers.not(Matchers.equalTo(new Data.ToPhi(7L).φTerm()))
        );
    }

    @Test
    void producesEqualTermForEqualNumbers() {
        MatcherAssert.assertThat(
            "Equal numbers must produce equal φ-terms, but they didnt",
            new Data.ToPhi(5L).φTerm(),
            Matchers.equalTo(new Data.ToPhi(5L).φTerm())
        );
    }

    @Test
    void comparesVertex() {
        MatcherAssert.assertThat(
            "Hash codes of two Data.ToPhi instances with the same value should differ, but they didn't",
            new Data.ToPhi(42L).hashCode(),
            Matchers.not(
                Matchers.equalTo(
                    new Data.ToPhi(42L).hashCode()
                )
            )
        );
    }

    @ParameterizedTest
    @MethodSource("toPhiData")
    void comparesTwoDatas(final Object value, final String message) {
        MatcherAssert.assertThat(
            message,
            new Data.ToPhi(value),
            Matchers.not(Matchers.equalTo(new Data.ToPhi(value)))
        );
    }

    private static Stream<Arguments> toPhiData() {
        return Stream.of(
            Arguments.of(
                1L, "Data.ToPhi instances with the same long value should differ, but they didn't"
            ),
            Arguments.of(
                "Welcome",
                "Data.ToPhi instances with the same string value should differ, but they didn't"
            ),
            Arguments.of(
                2.18d,
                "Data.ToPhi instances with the same double value should differ, but they didn't"
            ),
            Arguments.of(
                new byte[]{(byte) 0x00, (byte) 0x1f},
                "Data.ToPhi instances with the same byte array value should differ, but they didn't"
            )
        );
    }
}
