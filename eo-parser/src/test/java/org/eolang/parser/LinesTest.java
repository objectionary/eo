/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Arrays;
import java.util.Collections;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link Lines}.
 * @since 0.50
 */
final class LinesTest {

    @Test
    void readsTheFirstLine() {
        MatcherAssert.assertThat(
            "line 1 must be the first line of the source",
            new Lines(
                Arrays.asList(new TextOf("alpha"), new TextOf("beta"))
            ).line(1),
            Matchers.equalTo("alpha")
        );
    }

    @Test
    void readsTheLastLine() {
        MatcherAssert.assertThat(
            "the highest valid number must be the last line",
            new Lines(
                Arrays.asList(new TextOf("alpha"), new TextOf("beta"))
            ).line(2),
            Matchers.equalTo("beta")
        );
    }

    @Test
    void readsAGenuinelyEmptyLine() {
        MatcherAssert.assertThat(
            "an empty line at a valid number must come back as an empty string",
            new Lines(
                Arrays.asList(new TextOf("alpha"), new TextOf(""))
            ).line(2),
            Matchers.equalTo("")
        );
    }

    @ParameterizedTest
    @ValueSource(ints = {-1, 0, 3, 500, Integer.MAX_VALUE})
    void rejectsANumberThatIsNotALine(final int number) {
        Assertions.assertThrows(
            IndexOutOfBoundsException.class,
            () -> new Lines(
                Arrays.asList(new TextOf("alpha"), new TextOf("beta"))
            ).line(number),
            "a number outside the range of lines must be reported, not folded into an empty string"
        );
    }

    @Test
    void rejectsAnyNumberWhenThereAreNoLines() {
        Assertions.assertThrows(
            IndexOutOfBoundsException.class,
            () -> new Lines(Collections.emptyList()).line(1),
            "an empty source must have no line 1"
        );
    }

    @Test
    void namesTheNumberAndTheSizeWhenItRejects() {
        MatcherAssert.assertThat(
            "the message must name the number asked for and how many lines there are",
            Assertions.assertThrows(
                IndexOutOfBoundsException.class,
                () -> new Lines(
                    Arrays.asList(new TextOf("alpha"), new TextOf("beta"))
                ).line(500),
                "a number outside the range of lines must be reported"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("500"),
                Matchers.containsString("2")
            )
        );
    }
}
