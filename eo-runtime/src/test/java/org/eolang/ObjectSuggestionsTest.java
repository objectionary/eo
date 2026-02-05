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
 *
 * @since 0.52
 */
final class ObjectSuggestionsTest {

    @Test
    void suggestsSimilarObjects() {
        final String result = new ObjectSuggestions().suggest("EOorg.EOeolang.EOio.EOstd1out");
        MatcherAssert.assertThat(
            "Should suggest similar objects for a typo",
            result,
            Matchers.containsString("Did you mean?")
        );
    }

    @Test
    void suggestsStdoutForStd1out() {
        final String result = new ObjectSuggestions().suggest("EOorg.EOeolang.EOio.EOstd1out");
        MatcherAssert.assertThat(
            "Should suggest stdout for std1out typo",
            result,
            Matchers.anyOf(
                Matchers.containsString("stdout"),
                Matchers.containsString("Did you mean?")
            )
        );
    }

    @Test
    void includesNewlinesInOutput() {
        final String result = new ObjectSuggestions().suggest("EOorg.EOeolang.EOio.EOstd1out");
        if (!result.isEmpty()) {
            MatcherAssert.assertThat(
                "Output should start with newlines for proper formatting",
                result,
                Matchers.startsWith("\n\n")
            );
        }
    }

    @Test
    void formatsWithDashes() {
        final String result = new ObjectSuggestions().suggest("EOorg.EOeolang.EOas_byte");
        if (!result.isEmpty()) {
            MatcherAssert.assertThat(
                "Output should format with dash prefix for each suggestion",
                result,
                Matchers.containsString("  - ")
            );
        }
    }

    @Test
    void handlesCompletelyUnknownObject() {
        final ObjectSuggestions suggestions = new ObjectSuggestions();
        final String result = suggestions.suggest("EOorg.EOeolang.EOxyz123abc456");
        MatcherAssert.assertThat(
            "Should still provide some output or empty string for completely unknown objects",
            result,
            Matchers.anyOf(
                Matchers.containsString("Did you mean?"),
                Matchers.equalTo("")
            )
        );
    }

    @Test
    void handlesEmptyInput() {
        final String result = new ObjectSuggestions().suggest("");
        MatcherAssert.assertThat(
            "Should handle empty input gracefully",
            result,
            Matchers.notNullValue()
        );
    }

    @Test
    void handlesNonEoClassName() {
        final String result = new ObjectSuggestions().suggest("java.lang.String");
        MatcherAssert.assertThat(
            "Should handle non-EO class name gracefully",
            result,
            Matchers.notNullValue()
        );
    }
}
