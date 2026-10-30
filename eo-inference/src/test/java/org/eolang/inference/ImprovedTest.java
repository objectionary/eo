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
 * Test case for {@link Improved}.
 *
 * @since 0.73.5
 */
final class ImprovedTest {

    @Test
    void takesAVoidRootedAnswerWithFewerSteps() {
        MatcherAssert.assertThat(
            "an answer that drops a step the program never had must be taken, but it was refused",
            new Improved(
                Collections.singletonList("Φ.bool.if"), "Φ.bool.if.if.eq", "Φ.string.trimmed.α0"
            ).on("Φ.bool.if.eq"),
            Matchers.is(true)
        );
    }

    @Test
    void takesAShorterAnswerRootedAtAnotherVoid() {
        MatcherAssert.assertThat(
            "an answer worked out through another void must be taken when it is shorter",
            new Improved(
                Arrays.asList("Φ.bool.if", "Φ.number.φ"),
                "Φ.number.φ.as-bytes.size.eq.and.if",
                "Φ.tuple.at.α0"
            ).on("Φ.bool.if"),
            Matchers.is(true)
        );
    }

    @Test
    void keepsTheAnswerAgainstALongerOne() {
        MatcherAssert.assertThat(
            "an answer with more steps than the one on record must be refused, but it got in",
            new Improved(
                Collections.singletonList("Φ.bool.if"), "Φ.bool.if.eq", "Φ.string.eq.α0"
            ).on("Φ.bool.if.eq.not.if.eq"),
            Matchers.is(false)
        );
    }

    @Test
    void keepsTheAnswerAgainstAnotherOfTheSameLength() {
        MatcherAssert.assertThat(
            "two roads of one length to one call must leave the record alone, but it moved",
            new Improved(
                Arrays.asList("Φ.bool.if", "Φ.string.φ"), "Φ.bool.if.eq", "Φ.string.eq.α0"
            ).on("Φ.string.φ.eq"),
            Matchers.is(false)
        );
    }

    @Test
    void takesAnAnswerThatNamesAFormation() {
        MatcherAssert.assertThat(
            "an answer rooted at no void must replace a void-rooted one, but it was refused",
            new Improved(
                Collections.singletonList("Φ.bool.if"), "Φ.bool.if.eq", "Φ.number.abs.α0"
            ).on("Φ.number.as-bytes.size.eq.and.if.if"),
            Matchers.is(true)
        );
    }

    @Test
    void refusesTheAnswerAlreadyOnRecord() {
        MatcherAssert.assertThat(
            "an answer that repeats the record says nothing new, but it was taken again",
            new Improved(
                Collections.singletonList("Φ.bool.if"), "Φ.bool.if.eq", "Φ.string.eq.α0"
            ).on("Φ.bool.if.eq"),
            Matchers.is(false)
        );
    }
}
