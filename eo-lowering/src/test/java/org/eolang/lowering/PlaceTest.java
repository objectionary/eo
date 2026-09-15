/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link Place}.
 *
 * @since 0.77.0
 */
final class PlaceTest {

    @ParameterizedTest
    @ValueSource(
        strings = {
            "Φ.number.twice",
            "Φ.stdin.all-lines.a🌵48-4",
            "Φ.bool.p🌵can-nest-formations.blah0.blah1",
            "Φ.foo.Ω",
            "Φ"
        }
    )
    void readsBackWhatLambdaSpelt(final String locator) {
        MatcherAssert.assertThat(
            "the locator must survive the trip through its λ, but it didnt",
            new Place(new Lambda(locator).name()).name(),
            Matchers.equalTo(locator)
        );
    }

    @Test
    void readsBackALongLocator() {
        final String locator = Stream.concat(
            Stream.of("Φ"),
            Stream.generate(() -> "blah-blah").limit(40)
        ).collect(Collectors.joining("."));
        MatcherAssert.assertThat(
            "a long locator must survive the trip through its λ, but it didnt",
            new Place(new Lambda(locator).name()).name(),
            Matchers.equalTo(locator)
        );
    }

    @Test
    void refusesNameOfNoBox() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Place("L_number_plus").name(),
            "the name of an operation is not the name of a box, but it was read as one"
        );
    }
}
