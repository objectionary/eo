/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for EO object names by class resources.
 * @since 1.0
 */
final class PhObjectNamesTest {

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
    void ignoresSpecResource() {
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
