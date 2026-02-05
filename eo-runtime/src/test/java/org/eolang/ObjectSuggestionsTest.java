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
 * @since 0.52
 */
final class ObjectSuggestionsTest {

    @Test
    void suggestsSimilarObjects() {
        MatcherAssert.assertThat(
            "Should suggest similar objects for typo",
            new ObjectSuggestions().suggest("EOorg.EOeolang.EOio.EOstd1out"),
            Matchers.containsString("Did you mean?")
        );
    }

    @Test
    void formatsOutputCorrectly() {
        final String result = new ObjectSuggestions().suggest(
            "EOorg.EOeolang.EOio.EOstd1out"
        );
        MatcherAssert.assertThat(
            "Output should start with newlines",
            result,
            Matchers.startsWith("\n\n")
        );
        MatcherAssert.assertThat(
            "Output should contain dash prefix",
            result,
            Matchers.containsString("  - ")
        );
    }

    @Test
    void handlesEmptyInput() {
        MatcherAssert.assertThat(
            "Should handle empty input",
            new ObjectSuggestions().suggest(""),
            Matchers.notNullValue()
        );
    }

    @Test
    void handlesNonEoInput() {
        MatcherAssert.assertThat(
            "Should handle non-EO input",
            new ObjectSuggestions().suggest("java.lang.String"),
            Matchers.notNullValue()
        );
    }
}
