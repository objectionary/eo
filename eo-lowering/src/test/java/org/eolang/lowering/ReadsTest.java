/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Reads}.
 * @since 0.76.0
 */
final class ReadsTest {

    @Test
    void listsVoidsOfNestedArms() {
        MatcherAssert.assertThat(
            "a void read deep inside an arm must count among all the reads, but it doesnt",
            new Reads(ReadsTest.forked("sym:v1", "number:11-")).all(),
            Matchers.contains(0, 1)
        );
    }

    @Test
    void leavesVoidOfOneArmToThatArm() {
        MatcherAssert.assertThat(
            "a void one arm alone reads must not be declared above the fork, but it is",
            new Reads(ReadsTest.forked("sym:v1", "number:11-")).own(Collections.emptySet()),
            Matchers.contains(0)
        );
    }

    @Test
    void claimsVoidBothArmsRead() {
        MatcherAssert.assertThat(
            "a void both arms read must be declared above the fork, but it isnt",
            new Reads(ReadsTest.forked("sym:v1", "sym:v1")).own(Collections.emptySet()),
            Matchers.contains(0, 1)
        );
    }

    @Test
    void skipsVoidDeclaredAbove() {
        MatcherAssert.assertThat(
            "a void an enclosing block declared must not be declared again, but it is",
            new Reads(ReadsTest.forked("sym:v1", "sym:v1")).own(Collections.singleton(0)),
            Matchers.contains(1)
        );
    }

    @Test
    void claimsVoidOfArmsOfTwoForks() {
        MatcherAssert.assertThat(
            "a void two forks read, one arm each, must be declared above both, but it isnt",
            new Reads(
                new Protocol(
                    Arrays.asList(
                        new Fork(
                            "s1", "L_bool_if", "sym:v0",
                            new Protocol(Collections.emptyList(), "sym:v1", "number"),
                            new Protocol(Collections.emptyList(), "number:11-", "number")
                        ),
                        new Fork(
                            "s2", "L_bool_if", "sym:v0",
                            new Protocol(Collections.emptyList(), "number:22-", "number"),
                            new Protocol(Collections.emptyList(), "sym:v1", "number")
                        )
                    ),
                    "sym:s2",
                    "number"
                )
            ).own(Collections.emptySet()),
            Matchers.contains(0, 1)
        );
    }

    @Test
    void countsVoidsARepeatHandsOn() {
        MatcherAssert.assertThat(
            "a void a repeat hands on must count as read, but it doesnt",
            new Reads(
                new Protocol(Collections.emptyList(), Arrays.asList("number:11-", "sym:v1"))
            ).all(),
            Matchers.contains(1)
        );
    }

    private static Protocol forked(final String yes, final String not) {
        return new Protocol(
            Collections.singletonList(
                new Fork(
                    "s1", "L_bool_if", "sym:v0",
                    new Protocol(Collections.emptyList(), yes, "number"),
                    new Protocol(Collections.emptyList(), not, "number")
                )
            ),
            "sym:s1",
            "number"
        );
    }
}
