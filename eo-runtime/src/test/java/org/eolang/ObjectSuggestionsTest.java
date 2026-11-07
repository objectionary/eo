/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link ObjectSuggestions}.
 * @since 0.74.0
 */
final class ObjectSuggestionsTest {

    @Test
    void suggestsTheClosestObjectForATypo() {
        MatcherAssert.assertThat(
            "a typo must be answered with the closest real object name",
            new ObjectSuggestions(
                Thread.currentThread().getContextClassLoader()
            ).suggest("Φ.org.eolang.io.std1out"),
            Matchers.containsString("- stdout")
        );
    }

    @Test
    void prefixesSuggestionsWithDidYouMean() {
        MatcherAssert.assertThat(
            "suggestions must be introduced by a 'Did you mean?' heading",
            new ObjectSuggestions(
                Thread.currentThread().getContextClassLoader()
            ).suggest("Φ.org.eolang.io.std1out"),
            Matchers.startsWith(String.format("%n%nDid you mean?"))
        );
    }

    @Test
    void suggestsSomethingEvenForAnUnknownName() {
        MatcherAssert.assertThat(
            "an object far from every candidate must still be answered",
            new ObjectSuggestions(
                Thread.currentThread().getContextClassLoader()
            ).suggest("Φ.org.eolang.nothing-remotely-similar"),
            Matchers.containsString("Did you mean?")
        );
    }
}
