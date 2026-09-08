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
 *
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
    void countsAsNamedOnlyWhatStandsAboveTheVoids() {
        final Map<String, Integer> rungs = new LinkedHashMap<>(0);
        rungs.put("nothing", 1);
        rungs.put("a void", 1);
        rungs.put("a forma", 5);
        rungs.put("a whole forma", 1);
        MatcherAssert.assertThat(
            "six objects in eight know their forma, so three quarters must be named, but werent",
            new Ladder(rungs).named(),
            Matchers.closeTo(75.0d, 0.001d)
        );
    }

    @Test
    void keepsTheVoidRootedApartFromTheRest() {
        final Map<String, Integer> rungs = new LinkedHashMap<>(0);
        rungs.put("nothing", 1);
        rungs.put("a void", 3);
        rungs.put("a forma", 4);
        MatcherAssert.assertThat(
            "three objects in eight are known only through a void, but that wasnt the share",
            new Ladder(rungs).rooted(),
            Matchers.closeTo(37.5d, 0.001d)
        );
    }

    @Test
    void leavesTheBottomRungOnItsOwn() {
        final Map<String, Integer> rungs = new LinkedHashMap<>(0);
        rungs.put("nothing", 3);
        rungs.put("a void", 1);
        MatcherAssert.assertThat(
            "three objects in four are unknown, so that must be the blank share, but it wasnt",
            new Ladder(rungs).blank(),
            Matchers.closeTo(75.0d, 0.001d)
        );
    }

    @Test
    void addsTheThreeSharesUpToEverything() {
        final Map<String, Integer> rungs = new LinkedHashMap<>(0);
        rungs.put("nothing", 2);
        rungs.put("a void", 3);
        rungs.put("a forma", 7);
        rungs.put("a whole forma", 11);
        final Ladder ladder = new Ladder(rungs);
        MatcherAssert.assertThat(
            "every object stands in one band of three, so they must come to a hundred, but didnt",
            ladder.named() + ladder.rooted() + ladder.blank(),
            Matchers.closeTo(100.0d, 0.001d)
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
    void namesNothingOfAnEmptyProgram() {
        MatcherAssert.assertThat(
            "a program with no rungs at all names a zero share, but it threw",
            new Ladder(new LinkedHashMap<>(0)).named(),
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
