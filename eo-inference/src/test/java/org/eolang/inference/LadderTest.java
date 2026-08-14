/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Ladder}.
 * @since 0.69.0
 */
final class LadderTest {

    @Test
    void takesTheMeanRungAgainstTheHighestOne() {
        final Map<String, Integer> rungs = new LinkedHashMap<>(0);
        rungs.put("nothing", 7);
        rungs.put("something", 0);
        rungs.put("everything", 7);
        MatcherAssert.assertThat(
            "a program half of which is on the top rung must be half understood, but it wasnt",
            new Ladder(rungs).percent(),
            Matchers.closeTo(50.0d, 0.001d)
        );
    }

    @Test
    void leavesOutOnlyTheObjectsOnTheBottomRung() {
        final Map<String, Integer> rungs = new LinkedHashMap<>(0);
        rungs.put("nothing", 3);
        rungs.put("something", 1);
        MatcherAssert.assertThat(
            "one object in four is known, so a quarter must be described, but it wasnt",
            new Ladder(rungs).described(),
            Matchers.closeTo(25.0d, 0.001d)
        );
    }

    @Test
    void countsAnEmptyProgramWithoutFalling() {
        final Map<String, Integer> rungs = new LinkedHashMap<>(0);
        rungs.put("nothing", 0);
        rungs.put("something", 0);
        MatcherAssert.assertThat(
            "a program with no objects cannot be understood to any depth, but it was",
            new Ladder(rungs).percent(),
            Matchers.closeTo(0.0d, 0.001d)
        );
    }
}
