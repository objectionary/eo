/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhSuggestions}.
 * @since 1.0
 */
final class PhSuggestionsTest {

    /**
     * Max suggestions.
     */
    private static final int LIMIT = 5;

    @Test
    void ranksClosestObjectFirst() {
        MatcherAssert.assertThat(
            "The closest object must be suggested first",
            new PhSuggestions(
                Arrays.asList(
                    "io.stdin",
                    "io.stdout",
                    "tt.sprintf",
                    "tt.trimmed",
                    "tt.concat"
                )
            ).suggestions("Φ.io.std1out", PhSuggestionsTest.LIMIT).get(0),
            Matchers.equalTo("io.stdout")
        );
    }

    @Test
    void limitsSuggestions() {
        MatcherAssert.assertThat(
            "Suggestion list must be limited",
            new PhSuggestions(
                Arrays.asList(
                    "io.stdin",
                    "io.stdout",
                    "io.console",
                    "tt.sprintf",
                    "tt.trimmed",
                    "tt.concat"
                )
            ).suggestions("Φ.io.std1out", PhSuggestionsTest.LIMIT),
            Matchers.iterableWithSize(PhSuggestionsTest.LIMIT)
        );
    }

    @Test
    void omitsMessageWithoutCandidates() {
        MatcherAssert.assertThat(
            "Suggestions must not render a section without candidates",
            new PhSuggestions(Collections.emptyList()).message("Φ.io.std1out"),
            Matchers.equalTo("")
        );
    }

    @Test
    void preservesOrgEolangPrefix() {
        MatcherAssert.assertThat(
            "Suggestion must keep org.eolang prefix when the missing object uses it",
            new PhSuggestions(
                Arrays.asList("io.stdin", "io.stdout", "tt.sprintf")
            ).suggestions("Φ.org.eolang.io.std1out", PhSuggestionsTest.LIMIT).get(0),
            Matchers.equalTo("org.eolang.io.stdout")
        );
    }

    @Test
    void convertsAnnotatedResource() {
        MatcherAssert.assertThat(
            "Annotated classes must preserve their original EO names",
            PhSuggestions.names("org/eolang/EOtt/EOsprintf.class"),
            Matchers.contains("tt.sprintf")
        );
    }

    @Test
    void convertsPackageInfoResource() {
        MatcherAssert.assertThat(
            "Package markers must become package candidates",
            PhSuggestions.names("org/eolang/EOtt/package-info.class"),
            Matchers.contains("tt")
        );
    }

    @Test
    void convertsFallbackResource() {
        MatcherAssert.assertThat(
            "Fallback conversion must restore dashes and underscores",
            PhSuggestions.names("org/eolang/EOfoo__bar/EOas_bytes.class"),
            Matchers.contains("foo_bar.as-bytes")
        );
    }

    @Test
    void ignoresHelperResource() {
        MatcherAssert.assertThat(
            "Helper classes must not become suggestions",
            PhSuggestions.names("org/eolang/EOtt/SprintfArgs.class"),
            Matchers.emptyIterable()
        );
    }

    @Test
    void ignoresTestResource() {
        MatcherAssert.assertThat(
            "Test classes must not become suggestions",
            PhSuggestions.names("org/eolang/EOnumberTest.class"),
            Matchers.emptyIterable()
        );
    }

    @Test
    void ignoresInternalGeneratedResource() {
        MatcherAssert.assertThat(
            "Internal generated objects must not become suggestions",
            PhSuggestions.names(
                "org/eolang/EOio/EOmalloc_as_output$EOΦiomalloc_as_outputφα1.class"
            ),
            Matchers.emptyIterable()
        );
    }
}
