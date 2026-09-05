/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Fork}.
 * @since 0.76.0
 */
final class ForkTest {

    @Test
    void answersFormaOfArms() {
        MatcherAssert.assertThat(
            "a fork must compute what its arms compute, but it doesnt",
            new Fork(
                "s2", "L_bool_if", "sym:s1",
                new Protocol(Collections.emptyList(), "sym:v0", "number"),
                new Protocol(Collections.emptyList(), "number:40-00-", "number")
            ).forma(),
            Matchers.equalTo("number")
        );
    }

    @Test
    void refusesDisagreeingArms() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Fork(
                "s2", "L_bool_if", "sym:s1",
                new Protocol(Collections.emptyList(), "sym:v0", "number"),
                new Protocol(Collections.emptyList(), "bool:FF-", "bool")
            )::forma,
            "arms of two formas cannot share one value, but they did"
        );
    }

    @Test
    void answersWithArmThatDoesNotRepeat() {
        MatcherAssert.assertThat(
            "a fork repeating in one arm must answer what the other does, but it doesnt",
            new Fork(
                "s2", "L_bool_if", "sym:s1",
                new Protocol(Collections.emptyList(), Collections.singletonList("sym:v0")),
                new Protocol(Collections.emptyList(), "sym:v0", "number")
            ).forma(),
            Matchers.equalTo("number")
        );
    }

    @Test
    void answersNothingWhenBothArmsRepeat() {
        MatcherAssert.assertThat(
            "a fork whose both arms repeat has no value and must name no forma, but it did",
            new Fork(
                "s2", "L_bool_if", "sym:s1",
                new Protocol(Collections.emptyList(), Collections.singletonList("sym:v0")),
                new Protocol(Collections.emptyList(), Collections.singletonList("sym:v0"))
            ).forma(),
            Matchers.equalTo("")
        );
    }

    @Test
    void readsOnlyItsCondition() {
        MatcherAssert.assertThat(
            "the one key a fork reads directly must be its condition, but it isnt",
            new Fork(
                "s2", "L_bool_if", "sym:v1",
                new Protocol(Collections.emptyList(), "sym:v0", "number"),
                new Protocol(Collections.emptyList(), "sym:v0", "number")
            ).keys(),
            Matchers.contains("sym:v1")
        );
    }

    @Test
    void holdsTakenArmFirst() {
        MatcherAssert.assertThat(
            "the arm taken when the bool holds must come first, but it doesnt",
            new Fork(
                "s2", "L_bool_if", "sym:s1",
                new Protocol(Collections.emptyList(), "number:11-", "number"),
                new Protocol(Collections.emptyList(), "number:22-", "number")
            ).branches().get(0).answer(),
            Matchers.equalTo("number:11-")
        );
    }
}
