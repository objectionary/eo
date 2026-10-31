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
            new Band(Arrays.asList("Φ.kettle", "?can't-tell", "Φ.mug")).name(),
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
            "only voids an atom fills must be violet, but they werent",
            new Band(Collections.singletonList("?open-void-atom")).name(),
            Matchers.equalTo("atom")
        );
    }

    @Test
    void marksAVoidNobodyFills() {
        MatcherAssert.assertThat(
            "a void nobody fills next to one an atom fills must be slate, but it wasnt",
            new Band(Arrays.asList("?open-void-atom", "?open-void")).name(),
            Matchers.equalTo("unfilled")
        );
    }

    @Test
    void losesTrackOfAPlaceItCannotTell() {
        MatcherAssert.assertThat(
            "can't tell next to an open void must be amber, but it wasnt",
            new Band(Arrays.asList("?open-void", "?can't-tell", "T")).name(),
            Matchers.equalTo("lost")
        );
    }

    @Test
    void marksAPlaceNoCopyReached() {
        MatcherAssert.assertThat(
            "a place with no answer must be grey, but it wasnt",
            new Band(Collections.emptyList()).name(),
            Matchers.equalTo("unreached")
        );
    }

    @Test
    void marksASkippedPlaceAsUnreached() {
        MatcherAssert.assertThat(
            "a place the engine skipped must be grey, but it wasnt",
            new Band(Collections.singletonList("?skipped-regex")).name(),
            Matchers.equalTo("unreached")
        );
    }

    @Test
    void marksAFailingReadWhateverElseIsAnswered() {
        MatcherAssert.assertThat(
            "a read that fails in one copy must be red next to a name, but it wasnt",
            new Band(Arrays.asList("Φ.kettle", "!Φ.kettle.spout")).name(),
            Matchers.equalTo("fails")
        );
    }
}
