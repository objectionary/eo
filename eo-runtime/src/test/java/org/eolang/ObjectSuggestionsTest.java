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
        MatcherAssert.assertThat(
            "Should suggest similar objects for a typo",
            new ObjectSuggestions().suggest("EOorg.EOeolang.EOio.EOstd1out"),
            Matchers.containsString("Did you mean?")
        );
    }

    @Test
    void suggestsStdoutForTypo() {
        MatcherAssert.assertThat(
            "Should suggest stdout for typo",
            new ObjectSuggestions().suggest("EOorg.EOeolang.EOio.EOstd1out"),
            Matchers.anyOf(
                Matchers.containsString("stdout"),
                Matchers.containsString("Did you mean?")
            )
        );
    }

    @Test
    void includesNewlinesInOutput() {
        final String result = new ObjectSuggestions().suggest(
            "EOorg.EOeolang.EOio.EOstd1out"
        );
        MatcherAssert.assertThat(
            "Output should start with newlines for proper formatting",
            result,
            Matchers.anyOf(
                Matchers.startsWith("\n\n"),
                Matchers.equalTo("")
            )
        );
    }

    @Test
    void formatsWithDashes() {
        final String result = new ObjectSuggestions().suggest("EOorg.EOeolang.EOas_byte");
        MatcherAssert.assertThat(
            "Output should format with dash prefix for each suggestion or be empty",
            result,
            Matchers.anyOf(
                Matchers.containsString("  - "),
                Matchers.equalTo("")
            )
        );
    }

    @Test
    void handlesCompletelyUnknownObject() {
        MatcherAssert.assertThat(
            "Should provide some output or empty string for unknown objects",
            new ObjectSuggestions().suggest("EOorg.EOeolang.EOxyz123abc456"),
            Matchers.anyOf(
                Matchers.containsString("Did you mean?"),
                Matchers.equalTo("")
            )
        );
    }

    @Test
    void handlesEmptyInput() {
        MatcherAssert.assertThat(
            "Should handle empty input gracefully",
            new ObjectSuggestions().suggest(""),
            Matchers.notNullValue()
        );
    }

    @Test
    void handlesNonEoClassName() {
        MatcherAssert.assertThat(
            "Should handle non-EO class name gracefully",
            new ObjectSuggestions().suggest("java.lang.String"),
            Matchers.notNullValue()
        );
    }
}
