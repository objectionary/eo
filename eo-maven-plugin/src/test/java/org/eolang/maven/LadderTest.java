/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Ladder}.
 *
 * @since 0.76.0
 */
final class LadderTest {

    @Test
    void startsWithTheCountOfAllObjects() {
        MatcherAssert.assertThat(
            "the ladder must open with every object of the program, but it didnt",
            new Ladder(LadderTest.tally(13, 39)).iterator().next(),
            Matchers.equalTo("52 objects in all")
        );
    }

    @Test
    void writesTheShareOfEachBand() {
        MatcherAssert.assertThat(
            "the ladder must write the share of a band out of a hundred, but it didnt",
            new Ladder(LadderTest.tally(13, 39)),
            Matchers.hasItem("75.0 % in gaps")
        );
    }

    @Test
    void writesTheCountOfEachBand() {
        MatcherAssert.assertThat(
            "the ladder must write how many objects a band holds, but it didnt",
            new Ladder(LadderTest.tally(13, 39)),
            Matchers.hasItem("13 objects in named")
        );
    }

    @Test
    void sharesNothingOfAnEmptyProgram() {
        MatcherAssert.assertThat(
            "the ladder of a program with no objects must share nothing, but it didnt",
            new Ladder(LadderTest.tally(0, 0)),
            Matchers.hasItem("0.0 % in named")
        );
    }

    @Test
    void writesTheSharesBeforeTheCounts() {
        MatcherAssert.assertThat(
            "the ladder must write every share before any count, but it didnt",
            new Ladder(LadderTest.tally(7, 2)),
            Matchers.contains(
                "9 objects in all",
                "77.8 % in named",
                "22.2 % in gaps",
                "7 objects in named",
                "2 objects in gaps"
            )
        );
    }

    private static Map<String, Integer> tally(final int named, final int gaps) {
        final Map<String, Integer> tally = new LinkedHashMap<>(2);
        tally.put("named", named);
        tally.put("gaps", gaps);
        return tally;
    }
}
