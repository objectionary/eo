/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Band}.
 *
 * @since 0.71.0
 */
final class BandTest {

    @Test
    void namesAPlaceThatAnswersOneNameAlone() {
        MatcherAssert.assertThat(
            "one name and nothing else must be green, but it wasnt",
            new Band(Collections.singletonList("Φ.kettle")).name(),
            Matchers.equalTo("named")
        );
    }

    @Test
    void countsADatumAsAName() {
        MatcherAssert.assertThat(
            "a datum alone must be green, but it wasnt",
            new Band(Collections.singletonList("D")).name(),
            Matchers.equalTo("named")
        );
    }

    @Test
    void marksAGapNextToOneName() {
        MatcherAssert.assertThat(
            "one name with an error in other copies must be lime, but it wasnt",
            new Band(Arrays.asList("T", "Φ.kettle")).name(),
            Matchers.equalTo("gaps")
        );
    }

    @Test
    void pairsTwoNamesAlone() {
        MatcherAssert.assertThat(
            "two names and nothing else must be teal, but they werent",
            new Band(Arrays.asList("Φ.kettle", "Φ.mug")).name(),
            Matchers.equalTo("pair")
        );
    }

    @Test
    void mixesTwoNamesWithAGap() {
        MatcherAssert.assertThat(
            "two names with a gap must be blue, but they werent",
            new Band(Arrays.asList("Φ.kettle", "?unknown-loop", "Φ.mug")).name(),
            Matchers.equalTo("mixed")
        );
    }

    @Test
    void mixesThreeNames() {
        MatcherAssert.assertThat(
            "three names must be blue, but they werent",
            new Band(Arrays.asList("Φ.kettle", "Φ.mug", "Φ.ω.lid")).name(),
            Matchers.equalTo("mixed")
        );
    }

    @Test
    void marksAnErrorAlone() {
        MatcherAssert.assertThat(
            "only an error must be brown, but it wasnt",
            new Band(Collections.singletonList("T")).name(),
            Matchers.equalTo("error")
        );
    }

    @Test
    void marksAVoidOnlyAnAtomFills() {
        MatcherAssert.assertThat(
            "only voids an atom fills must be purple, but they werent",
            new Band(Collections.singletonList("?unknown-java")).name(),
            Matchers.equalTo("atom")
        );
    }

    @Test
    void marksAReadNothingEverAnswers() {
        MatcherAssert.assertThat(
            "a read nothing ever answers must be pink, but it wasnt",
            new Band(Collections.singletonList("?unknown-loop")).name(),
            Matchers.equalTo("loop")
        );
    }

    @Test
    void marksAVoidOfAnObjectNobodyCalls() {
        MatcherAssert.assertThat(
            "a void of an object nobody calls must be slate, but it wasnt",
            new Band(Collections.singletonList("?unknown-study")).name(),
            Matchers.equalTo("study")
        );
    }

    @Test
    void marksAVoidNobodyFillsNextToAnError() {
        MatcherAssert.assertThat(
            "a void nobody fills next to an error must be amber, but it wasnt",
            new Band(Arrays.asList("T", "?unknown-empty")).name(),
            Matchers.equalTo("empty")
        );
    }

    @Test
    void putsALoopAheadOfAVoidAnAtomFills() {
        MatcherAssert.assertThat(
            "a loop next to a void an atom fills must be pink, but it wasnt",
            new Band(Arrays.asList("?unknown-java", "?unknown-loop")).name(),
            Matchers.equalTo("loop")
        );
    }

    @Test
    void putsAVoidAnAtomFillsAheadOfAnObjectNobodyCalls() {
        MatcherAssert.assertThat(
            "a void an atom fills next to one nobody calls must be purple, but it wasnt",
            new Band(Arrays.asList("?unknown-study", "?unknown-java")).name(),
            Matchers.equalTo("atom")
        );
    }

    @Test
    void putsAnObjectNobodyCallsAheadOfAVoidNobodyFills() {
        MatcherAssert.assertThat(
            "a void of an object nobody calls next to one nobody fills must be slate, but it wasnt",
            new Band(Arrays.asList("?unknown-empty", "?unknown-study")).name(),
            Matchers.equalTo("study")
        );
    }
}
