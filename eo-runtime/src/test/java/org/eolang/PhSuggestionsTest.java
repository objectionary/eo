/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Isolated;

/**
 * Test case for {@link PhSuggestions}.
 * @since 1.0
 */
@SuppressWarnings("PMD.TooManyMethods")
@Isolated
final class PhSuggestionsTest {

    /**
     * Java classpath system property.
     */
    private static final String CLASSPATH = "java.class.path";

    /**
     * Fixture artifact marker.
     */
    private static final String FIXTURE = "eo-suggestions-fixture";

    @Test
    void ranksClosestObjectFirst() {
        MatcherAssert.assertThat(
            "Closest and equal-score suggestions must be ranked predictably",
            Arrays.asList(
                new PhSuggestions(
                    Arrays.asList(
                        "io.stdin",
                        "io.stdout",
                        "tt.sprintf",
                        "tt.trimmed",
                        "tt.concat"
                    )
                ).suggestions("Φ.io.std1out", 5).get(0),
                new PhSuggestions(
                    Arrays.asList("aa.aby", "aa.abx")
                ).suggestions("Φ.aa.abc", 5)
            ),
            Matchers.contains("io.stdout", Arrays.asList("aa.abx", "aa.aby"))
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void discoversObjectsFromDependency() {
        final String origin = System.getProperty(PhSuggestionsTest.CLASSPATH);
        try {
            System.setProperty(
                PhSuggestionsTest.CLASSPATH,
                PhSuggestionsTest.fixture(System.getProperty(PhSuggestionsTest.CLASSPATH, ""))
            );
            MatcherAssert.assertThat(
                "Default suggestions must discover objects from dependency classpath entries",
                new PhSuggestions().suggestions("Φ.deps.prnter", 5),
                Matchers.hasItem("deps.printer")
            );
        } finally {
            PhSuggestionsTest.restore(origin);
        }
    }

    @Test
    void limitsSuggestions() {
        MatcherAssert.assertThat(
            "Suggestion list must be limited",
            new PhSuggestions(
                Arrays.asList(
                    "io.stdout",
                    "io.stdouts",
                    "io.stdout-as-bytes",
                    "io.stdout-line",
                    "io.stdout-length",
                    "io.stdout-text"
                )
            ).suggestions("Φ.io.std1out", 5),
            Matchers.iterableWithSize(5)
        );
    }

    @Test
    void omitsMessageWithoutCandidates() {
        MatcherAssert.assertThat(
            "Suggestions must not render a section without candidates",
            Arrays.asList(
                new PhSuggestions(Collections.emptyList()).message("Φ.io.std1out"),
                new PhSuggestions(
                    Arrays.asList("zz.qqq", "aa.bbb", "unrelated.object")
                ).message("Φ.io.std1out")
            ),
            Matchers.contains("", "")
        );
    }

    @Test
    void preservesOrgEolangPrefix() {
        MatcherAssert.assertThat(
            "Suggestion must keep org.eolang prefix when the missing object uses it",
            new PhSuggestions(
                Arrays.asList("io.stdin", "io.stdout", "tt.sprintf")
            ).suggestions("Φ.org.eolang.io.std1out", 5).get(0),
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

    /**
     * Find fixture classpath entry.
     * @param classpath Classpath
     * @return Fixture entry
     */
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    private static String fixture(final String classpath) {
        final String separator = Pattern.quote(File.pathSeparator);
        final List<String> entries = Arrays.stream(classpath.split(separator))
            .filter(item -> item.contains(PhSuggestionsTest.FIXTURE))
            .collect(Collectors.toList());
        return entries.stream()
            .filter(item -> item.endsWith(".jar"))
            .findFirst()
            .orElseGet(() -> PhSuggestionsTest.first(entries, classpath));
    }

    /**
     * First fixture entry.
     * @param entries Entries
     * @param classpath Classpath
     * @return Fixture entry
     */
    private static String first(final List<String> entries, final String classpath) {
        return entries.stream()
            .findFirst()
            .orElseThrow(() -> PhSuggestionsTest.absent(classpath));
    }

    /**
     * Absent fixture error.
     * @param classpath Classpath
     * @return Error
     */
    private static IllegalStateException absent(final String classpath) {
        return new IllegalStateException(
            String.format(
                "Classpath doesn't contain '%s': %s",
                PhSuggestionsTest.FIXTURE,
                classpath
            )
        );
    }

    /**
     * Restore classpath property.
     * @param origin Original value
     */
    private static void restore(final String origin) {
        if (origin == null) {
            System.clearProperty(PhSuggestionsTest.CLASSPATH);
        } else {
            System.setProperty(PhSuggestionsTest.CLASSPATH, origin);
        }
    }
}
