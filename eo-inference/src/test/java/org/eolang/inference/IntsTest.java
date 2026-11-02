/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.stream.IntStream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Ints}.
 *
 * @since 0.76.0
 */
final class IntsTest {

    @Test
    void keepsEachNumberOnce() {
        final Ints ints = new Ints(17, 4);
        ints.add(17);
        MatcherAssert.assertThat(
            "a number added again must not be kept twice, but it was",
            ints.size(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void keepsNumbersInOrderTheyCame() {
        MatcherAssert.assertThat(
            "the numbers must come back in the order they came, but they didnt",
            new Ints(905, 3, 61).array(),
            Matchers.equalTo(new int[] {905, 3, 61})
        );
    }

    @Test
    void findsNumberAmongMany() {
        MatcherAssert.assertThat(
            "a number among hundreds must be found, but it wasnt",
            new Ints(IntStream.range(0, 300).map(num -> num * 7 + 2).toArray()).contains(1703),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotFindNumberNeverAdded() {
        MatcherAssert.assertThat(
            "a number never added must not be found, but it was",
            new Ints(IntStream.range(0, 300).map(num -> num * 7 + 2).toArray()).contains(1704),
            Matchers.is(false)
        );
    }

    @Test
    void forgetsNumberTakenOut() {
        final Ints ints = new Ints(IntStream.range(0, 40).toArray());
        ints.remove(23);
        MatcherAssert.assertThat(
            "a number taken out must not be found, but it was",
            ints.contains(23),
            Matchers.is(false)
        );
    }

    @Test
    void freezesIntoSortedAnswer() {
        MatcherAssert.assertThat(
            "the frozen numbers must be the same answer in ascending order, but they werent",
            new Ints(88, 2, 41).frozen(),
            Matchers.hasToString(new Ids(2, 41, 88).toString())
        );
    }

    @Test
    void copiesWithOneMoreNumber() {
        MatcherAssert.assertThat(
            "the copy must hold the numbers and the one more, but it didnt",
            new Ints(61, 5).with(270).frozen(),
            Matchers.hasToString(new Ids(5, 61, 270).toString())
        );
    }
}
