/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.Func;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests from {@link TjsForeign}.
 *
 * @since 0.29.5
 */
final class TjsForeignTest {

    @ParameterizedTest
    @CsvSource({
        "abs",
        "org.eolang.int",
        "Q.io.stdout"
    })
    void contains(final String name) {
        final TjsForeign tojos = new TjsForeign();
        tojos.add(name);
        MatcherAssert.assertThat(
            "Tojo must contain the name, but it doesn't",
            tojos.contains(name),
            Matchers.is(true)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "abs, sba",
        "org.eolang.int, org.eolang.float",
        "Q.io.stdout, Q.string.sprintf"
    })
    void doesNotContain(final String existing, final String considered) {
        final TjsForeign tojos = new TjsForeign();
        tojos.add(existing);
        MatcherAssert.assertThat(
            "Tojo must not contain the name, but it doesn't",
            tojos.contains(considered),
            Matchers.is(false)
        );
    }

    @Test
    void findsLookingTojoCorrectly() {
        final String looking = "looking";
        final TjsForeign tojos = new TjsForeign();
        MatcherAssert.assertThat(
            "Found tojo should be the same as added",
            tojos.add(looking),
            Matchers.equalTo(tojos.find(looking))
        );
    }

    @Test
    void throwsExceptionIfTojoWasNotFound() {
        final String id = "absent";
        final TjsForeign tojos = new TjsForeign();
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> tojos.find(id),
            String.format("Should throw an exception if tojo with id='%s' was not found", id)
        );
    }

    @Test
    void findsAnyTojoIfSeveralTojosWithTheSameIdWereAdded() {
        final String same = "same";
        final TjsForeign tojos = new TjsForeign();
        MatcherAssert.assertThat(
            "We don't care which tojo will be returned, but it should be one of the added tojos",
            Arrays.asList(tojos.add(same), tojos.add(same)),
            Matchers.hasItem(tojos.find(same))
        );
    }

    @ParameterizedTest
    @MethodSource("tojoFunctionsWithoutDefaultValues")
    void throwsExceptionIfKeyWasNotFoundInTojo(
        final String key,
        final Func<TjForeign, Object> method
    ) {
        final TjForeign tojo = new TjsForeign().add("string");
        Assertions.assertThrows(
            AttributeNotFoundException.class,
            () -> method.apply(tojo),
            String.format("Should throw an exception if key='%s' was not found in Tojo", key)
        );
    }

    @Test
    void getsExceptionMessageIfKeyWasNotFoundInTojo() {
        Assertions.assertEquals(
            "There is no 'XMIR' attribute in the tojo",
            Assertions.assertThrows(
                AttributeNotFoundException.class,
                new TjsForeign().add("string")::xmir
            ).getMessage(),
            "Should throw an exception if key 'XMIR' was not found in Tojo"
        );
    }

    @ParameterizedTest
    @MethodSource("tojoFunctionsWithDefaultValues")
    void doesNotThrowsAnException(
        final String key,
        final Func<TjForeign, ?> method
    ) throws Exception {
        Assertions.assertEquals(
            method.apply(new TjsForeign().add("string")),
            key,
            String.format("Shouldn't throw an exception if key='%s' was found in Tojo", key)
        );
    }

    @Test
    void selectsSortedTojos() {
        final TjsForeign tojos = new TjsForeign();
        tojos.add("foo");
        tojos.add("bar");
        tojos.add("xyz");
        tojos.add("abc");
        MatcherAssert.assertThat(
            "Tojos are not sorted as expected",
            tojos.all().stream().map(TjForeign::identifier).collect(Collectors.toList()),
            Matchers.contains("abc", "bar", "foo", "xyz")
        );
    }

    private static Stream<Arguments> tojoFunctionsWithoutDefaultValues() {
        return Stream.of(
            Arguments.of("XMIR", (Func<TjForeign, Object>) TjForeign::xmir),
            Arguments.of("EO", (Func<TjForeign, Object>) TjForeign::source),
            Arguments.of("VERSION", (Func<TjForeign, Object>) TjForeign::version),
            Arguments.of("ID", (Func<TjForeign, Object>) TjForeign::description),
            Arguments.of("HASH", (Func<TjForeign, Object>) TjForeign::hash),
            Arguments.of("PROBED", (Func<TjForeign, Object>) TjForeign::probed)
        );
    }

    private static Stream<Arguments> tojoFunctionsWithDefaultValues() {
        return Stream.of(
            Arguments.of("string", (Func<TjForeign, Object>) TjForeign::identifier),
            Arguments.of("compile", (Func<TjForeign, Object>) TjForeign::scope)
        );
    }
}
