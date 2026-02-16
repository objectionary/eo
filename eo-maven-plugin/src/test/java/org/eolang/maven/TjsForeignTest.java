/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.Func;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
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
@SuppressWarnings("PMD.TooManyMethods")
final class TjsForeignTest {

    /**
     * Testable foreign tojos.
     */
    private TjsForeign tojos;

    /**
     * Set up environment before each test.
     */
    @BeforeEach
    void setUp() {
        this.tojos = new TjsForeign();
    }

    @ParameterizedTest
    @CsvSource({
        "abs",
        "org.eolang.int",
        "QQ.io.stdout"
    })
    void contains(final String name) {
        this.tojos.add(name);
        MatcherAssert.assertThat(
            "Tojo must contain the name, but it doesn't",
            this.tojos.contains(name),
            Matchers.is(true)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "abs, sba",
        "org.eolang.int, org.eolang.float",
        "QQ.io.stdout, QQ.tt.sprintf"
    })
    void doesNotContain(final String existing, final String considered) {
        this.tojos.add(existing);
        MatcherAssert.assertThat(
            "Tojo must not contain the name, but it doesn't",
            this.tojos.contains(considered),
            Matchers.is(false)
        );
    }

    @Test
    void findsLookingTojoCorrectly() {
        final String looking = "looking";
        MatcherAssert.assertThat(
            "Found tojo should be the same as added",
            this.tojos.add(looking),
            Matchers.equalTo(this.tojos.find(looking))
        );
    }

    @Test
    void throwsExceptionIfTojoWasNotFound() {
        final String id = "absent";
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> this.tojos.find(id),
            String.format("Should throw an exception if tojo with id='%s' was not found", id)
        );
    }

    @Test
    void findsAnyTojoIfSeveralTojosWithTheSameIdWereAdded() {
        final String same = "same";
        final TjForeign first = this.tojos.add(same);
        final TjForeign second = this.tojos.add(same);
        final List<TjForeign> expected = Arrays.asList(first, second);
        MatcherAssert.assertThat(
            "We don't care which tojo will be returned, but it should be one of the added tojos",
            expected,
            Matchers.hasItem(this.tojos.find(same))
        );
    }

    @ParameterizedTest
    @MethodSource("tojoFunctionsWithoutDefaultValues")
    void throwsExceptionIfKeyWasNotFoundInTojo(
        final String key,
        final Func<TjForeign, Object> method) {
        final TjForeign tojo = this.tojos.add("string");
        Assertions.assertThrows(
            AttributeNotFoundException.class,
            () -> method.apply(tojo),
            String.format("Should throw an exception if key='%s' was not found in Tojo", key)
        );
    }

    @Test
    void getsExceptionMessageIfKeyWasNotFoundInTojo() {
        final TjForeign tojo = this.tojos.add("string");
        final AttributeNotFoundException thrown = Assertions.assertThrows(
            AttributeNotFoundException.class,
            tojo::xmir
        );
        Assertions.assertEquals(
            "There is no 'XMIR' attribute in the tojo",
            thrown.getMessage(),
            "Should throw an exception if key 'XMIR' was not found in Tojo"
        );
    }

    @ParameterizedTest
    @MethodSource("tojoFunctionsWithDefaultValues")
    void doesNotThrowsAnException(
        final String key,
        final Func<TjForeign, ?> method
    ) throws Exception {
        final TjForeign tojo = this.tojos.add("string");
        Assertions.assertEquals(
            method.apply(tojo),
            key,
            String.format("Shouldn't throw an exception if key='%s' was found in Tojo", key)
        );
    }

    @Test
    void selectsSortedTojos() {
        this.tojos.add("foo");
        this.tojos.add("bar");
        this.tojos.add("xyz");
        this.tojos.add("abc");
        MatcherAssert.assertThat(
            "Tojos are not sorted as expected",
            this.tojos.all().stream().map(TjForeign::identifier).collect(Collectors.toList()),
            Matchers.contains("abc", "bar", "foo", "xyz")
        );
    }

    @AfterEach
    void tearDown() throws IOException {
        this.tojos.close();
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
