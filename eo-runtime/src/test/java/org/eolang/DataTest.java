/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.nio.charset.StandardCharsets;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link Data}.
 *
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
    void doesNotNeedRho() {
        MatcherAssert.assertThat(
            "Data must arrive with its receiver in place, but it didnt",
            new Data.ToPhi("hello").needsRho(),
            Matchers.is(false)
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
    void failsFastWhenDataIsNull() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Data.ToPhi(null),
            "null data was converted instead of rejected"
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

    @ParameterizedTest
    @ValueSource(doubles = {42.0, 0.0, -0.0})
    void dataizesFiniteNumbersToTheirOriginalBytes(final double value) {
        Assertions.assertArrayEquals(
            new BytesOf(value).take(), new Dataized(new Data.ToPhi(value)).take(),
            "finite number bytes must remain unchanged"
        );
    }

    @Test
    void dataizesUnicodeString() {
        final String text = "雪だるま ☃";
        Assertions.assertArrayEquals(
            text.getBytes(StandardCharsets.UTF_8),
            new Dataized(new Data.ToPhi(text)).take(),
            "Unicode string bytes must remain UTF-8"
        );
    }

    @Test
    void isolatesByteArrayInputAndOutput() {
        final byte[] source = {1, 2, 3};
        final Phi data = new Data.ToPhi(source);
        source[0] = 9;
        final byte[] first = new Dataized(data).take();
        first[1] = 9;
        Assertions.assertArrayEquals(
            new byte[] {1, 2, 3}, new Dataized(data).take(),
            "Data.ToPhi must isolate both input and returned arrays"
        );
    }

    @Test
    void keepsSpecialNumberObjectsOnNamedRoots() {
        MatcherAssert.assertThat(
            "special numbers must retain their named roots",
            Stream.of(
                Double.NaN, Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY
            ).map(number -> new Data.ToPhi(number).φTerm()).toList(),
            Matchers.contains("nan", "pinf", "ninf")
        );
    }

    @Test
    void exposesLiteralWrapperOnlyForFiniteData() {
        MatcherAssert.assertThat(
            "only finite scalar data must expose the literal wrapper",
            Stream.of(
                new Data.ToPhi(42L),
                new Data.ToPhi("text"),
                new Data.ToPhi(new byte[] {1}),
                new Data.ToPhi(true),
                new Data.ToPhi(Double.NaN)
            ).map(Phi::copy).map(PhLiteral.class::isInstance).toList(),
            Matchers.contains(true, true, true, false, false)
        );
    }

    @Test
    void keepsNumberObjectOperationsAfterCopying() {
        final Phi original = new Data.ToPhi(42L);
        final Phi copy = original.copy();
        MatcherAssert.assertThat(
            "a copied number must preserve its delegated object operations",
            Stream.<Object>of(
                copy.forma(), copy.needsRho(), copy.φTerm(),
                copy.take("as-decimal").φTerm(), copy.normalized().φTerm()
            ).toList(),
            Matchers.equalTo(
                Stream.<Object>of(
                    original.forma(), original.needsRho(), original.φTerm(),
                    original.take("as-decimal").φTerm(),
                    original.normalized().φTerm()
                ).toList()
            )
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
    void printsStringWithSpecialCharactersAsTerm() {
        MatcherAssert.assertThat(
            "String with quotes, backslashes and control chars must render escaped, but it didnt",
            new Data.ToPhi(String.format("a\"b\\c%cd%ce%cf", 0x09, 0x0D, 0x0A)).φTerm(),
            Matchers.equalTo("\"a\\\"b\\\\c\\td\\re\\nf\"")
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

    @Test
    void comparesVertexWithFormation() {
        MatcherAssert.assertThat(
            "Hash code of a formation should differ from the one of a data object, but it didn't",
            new PhDefault().hashCode(),
            Matchers.not(new Data.ToPhi(0L).hashCode())
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
                new byte[]{(byte) 0x00, (byte) 0x1F},
                "Data.ToPhi instances with the same byte array value should differ, but they didn't"
            )
        );
    }
}
