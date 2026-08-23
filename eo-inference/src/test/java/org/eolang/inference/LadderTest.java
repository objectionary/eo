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

    @Test
    void describesAnEmptyProgramAsZero() {
        MatcherAssert.assertThat(
            "a program with no rungs at all has a zero described share, but it threw",
            new Ladder(new LinkedHashMap<>(0)).described(),
            Matchers.closeTo(0.0d, 0.001d)
        );
    }

    @Test
    void takesAZeroDepthForASingleRung() {
        final Map<String, Integer> rungs = new LinkedHashMap<>(0);
        rungs.put("nothing", 5);
        MatcherAssert.assertThat(
            "a ladder with a single rung has nothing to climb, but it was NaN",
            new Ladder(rungs).percent(),
            Matchers.closeTo(0.0d, 0.001d)
        );
    }

    @Test
    void takesAZeroDepthForNoRungsAtAll() {
        MatcherAssert.assertThat(
            "a ladder with no rungs at all has a zero depth, but it wasnt",
            new Ladder(new LinkedHashMap<>(0)).percent(),
            Matchers.closeTo(0.0d, 0.001d)
        );
    }

    @Test
    void keepsTheRungsUnaffectedByLaterChangesToTheSourceMap() {
        final Map<String, Integer> rungs = new LinkedHashMap<>(0);
        rungs.put("nothing", 1);
        final Ladder ladder = new Ladder(rungs);
        rungs.put("something", 99);
        MatcherAssert.assertThat(
            "the rungs handed out must not change when the source map is mutated later, but they did",
            ladder.rungs().keySet(),
            Matchers.not(Matchers.hasItem("something"))
        );
    }
}
