/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Program}.
 * @since 0.76.0
 */
final class ProgramTest {

    @Test
    void listsFormasOfAllBodiesByPosition() {
        MatcherAssert.assertThat(
            "the voids of every body must stand at the positions the bodies know, but they dont",
            ProgramTest.bouncing().formas(),
            Matchers.contains("number", "number", "bytes")
        );
    }

    @Test
    void numbersBodiesFromTheFormation() {
        MatcherAssert.assertThat(
            "a helper body must be numbered after the formation's own, but it isnt",
            ProgramTest.bouncing().index("a🌵3-4"),
            Matchers.equalTo(1)
        );
    }

    @Test
    void answersFormaOfTheBodyThatAnswers() {
        MatcherAssert.assertThat(
            "the program must answer what its answering body answers, but it doesnt",
            ProgramTest.bouncing().carrier(),
            Matchers.equalTo("number")
        );
    }

    @Test
    void refusesProgramThatNeverAnswers() {
        MatcherAssert.assertThat(
            "a program whose bodies only resume one another never answers and must refuse",
            Assertions.assertThrows(
                IllegalStateException.class,
                new Program(
                    Arrays.asList(
                        new Body(
                            "", 0, Collections.singletonList("number"),
                            new Protocol(
                                Collections.emptyList(), "a🌵3-4",
                                Collections.singletonList("sym:v0")
                            )
                        ),
                        new Body(
                            "a🌵3-4", 1, Collections.singletonList("number"),
                            new Protocol(
                                Collections.emptyList(), "a🌵3-4",
                                Collections.singletonList("sym:v1")
                            )
                        )
                    ),
                    Collections.singletonMap("x", "number")
                )::carrier,
                "a program that never answers was given a forma, but it must not be"
            ).getMessage(),
            Matchers.containsString("never answers")
        );
    }

    private static Program bouncing() {
        return new Program(
            Arrays.asList(
                new Body(
                    "", 0, Collections.singletonList("number"),
                    new Protocol(
                        Collections.emptyList(), "a🌵3-4",
                        Arrays.asList("sym:v0", "bytes:01-")
                    )
                ),
                new Body(
                    "a🌵3-4", 1, Arrays.asList("number", "bytes"),
                    new Protocol(Collections.emptyList(), "sym:v1", "number")
                )
            ),
            Collections.singletonMap("x", "number")
        );
    }
}
