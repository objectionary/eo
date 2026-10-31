/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.nio.IntBuffer;
import java.util.HashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Ids}.
 *
 * @since 0.76.0
 */
final class IdsTest {

    @Test
    void holdsEveryNumberOfSmallerSet() {
        MatcherAssert.assertThat(
            "an answer must hold a set whose numbers are all in it, but it didnt",
            new Ids(3, 19, 230).holds(new Ints(230, 3)),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotHoldSetWithNumberOfItsOwn() {
        MatcherAssert.assertThat(
            "an answer must not hold a set with a number it lacks, but it did",
            new Ids(3, 19, 230).holds(new Ints(19, 20)),
            Matchers.is(false)
        );
    }

    @Test
    void growsByNumbersOfAnotherSet() {
        MatcherAssert.assertThat(
            "the answer with another set added must hold the numbers of both, but it didnt",
            new Ids(5, 77).with(new Ints(12, 77)),
            Matchers.hasToString(new Ids(5, 12, 77).toString())
        );
    }

    @Test
    void findsNumberItHolds() {
        MatcherAssert.assertThat(
            "a number of the answer must be found in it, but it wasnt",
            new Ids(1, 8, 64, 512).contains(64),
            Matchers.is(true)
        );
    }

    @Test
    void givesAnswerOfPoolWithSameValues() {
        final Map<IntBuffer, Ids> pool = new HashMap<>(0);
        MatcherAssert.assertThat(
            "an answer with the values of one in the pool must be that one, but it wasnt",
            new Ids(4, 17, 903).in(pool),
            Matchers.sameInstance(new Ids(4, 17, 903).in(pool))
        );
    }
}
