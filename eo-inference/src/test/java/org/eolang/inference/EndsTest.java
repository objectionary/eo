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
 * Test case for {@link Ends}.
 * @since 0.68.0
 */
final class EndsTest {

    @Test
    void walksAChainToItsEnd() {
        MatcherAssert.assertThat(
            "every name on a chain must arrive at the one the chain ends with, but it didnt",
            new Ends(this.pairs("a", "b", "b", "c")).names(),
            Matchers.allOf(
                Matchers.hasEntry("a", "c"),
                Matchers.hasEntry("b", "c")
            )
        );
    }

    @Test
    void givesOneNameToBothSidesOfATwoNameRing() {
        MatcherAssert.assertThat(
            "two names that are copies of each other are one type and must answer with one name, but they didnt",
            new Ends(this.pairs("a", "b", "b", "a")).names(),
            Matchers.allOf(
                Matchers.hasEntry("a", "a"),
                Matchers.hasEntry("b", "a")
            )
        );
    }

    @Test
    void givesOneNameToAWholeRing() {
        MatcherAssert.assertThat(
            "a ring of three copies must answer with the same name wherever it is entered, but it didnt",
            new Ends(this.pairs("b", "c", "c", "a", "a", "b")).names(),
            Matchers.allOf(
                Matchers.hasEntry("a", "a"),
                Matchers.hasEntry("b", "a"),
                Matchers.hasEntry("c", "a")
            )
        );
    }

    @Test
    void leadsAChainIntoTheNameOfTheRingItReaches() {
        MatcherAssert.assertThat(
            "a chain that runs into a ring must answer with the name of that ring, but it didnt",
            new Ends(this.pairs("d", "b", "b", "c", "c", "b")).names(),
            Matchers.hasEntry("d", "b")
        );
    }

    @Test
    void keepsANameThatIsACopyOfItself() {
        MatcherAssert.assertThat(
            "a name that is a copy of itself is nothing new and must answer with itself, but it didnt",
            new Ends(this.pairs("a", "a")).names(),
            Matchers.hasEntry("a", "a")
        );
    }

    @Test
    void walksOneNameToTheEndOfItsChain() {
        MatcherAssert.assertThat(
            "a name asked on its own must arrive where the whole chain arrives, but it didnt",
            new Ends(this.pairs("a", "b", "b", "c")).name("a"),
            Matchers.equalTo("c")
        );
    }

    @Test
    void answersForARingWhicheverOfItsNamesIsAsked() {
        MatcherAssert.assertThat(
            "a ring asked at one of its names must answer with the name the ring goes by, but it didnt",
            new Ends(this.pairs("b", "c", "c", "a", "a", "b")).name("c"),
            Matchers.equalTo("a")
        );
    }

    @Test
    void keepsANameTheTableSaysNothingAbout() {
        MatcherAssert.assertThat(
            "a name that is a copy of nothing must come back as it went in, but it didnt",
            new Ends(this.pairs("a", "b")).name("z"),
            Matchers.equalTo("z")
        );
    }

    private Map<String, String> pairs(final String... flat) {
        final Map<String, String> pairs = new LinkedHashMap<>(flat.length / 2);
        for (int idx = 0; idx < flat.length; idx = idx + 2) {
            pairs.put(flat[idx], flat[idx + 1]);
        }
        return pairs;
    }
}
