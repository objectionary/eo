/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.stream.Collectors;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Isolated;

/**
 * Test case for {@link PhSuggestions}.
 * @since 1.0
 */
@SuppressWarnings("PMD.TooManyMethods")
@Isolated
final class PhSuggestionsTest {

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
        final String origin = System.getProperty(PhSuggestionsTest.classpath());
        final String fixture = PhSuggestionsTest.fixture();
        try {
            System.setProperty(PhSuggestionsTest.classpath(), fixture);
            MatcherAssert.assertThat(
                "Default suggestions must discover objects from dependency classpath entries",
                String.format(
                    "%s%n%s",
                    fixture,
                    new PhSuggestions().suggestions("Φ.deps.prnter", 5)
                ),
                Matchers.allOf(
                    Matchers.containsString(PhSuggestionsTest.marker()),
                    Matchers.containsString("deps.printer")
                )
            );
        } finally {
            PhSuggestionsTest.restore(origin);
        }
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void discoversObjectsFromArbitraryDependencyJars(
        @TempDir final Path temp
    ) throws IOException {
        final String origin = System.getProperty(PhSuggestionsTest.classpath());
        final String first = PhSuggestionsTest.unique(temp, "first");
        final String second = PhSuggestionsTest.unique(temp, "second");
        final List<Path> jars = Arrays.asList(
            PhSuggestionsTest.dependency(temp, "first.jar", String.format("%s.printer", first)),
            PhSuggestionsTest.dependency(temp, "second.jar", String.format("%s.logger", second))
        );
        try {
            System.setProperty(PhSuggestionsTest.classpath(), PhSuggestionsTest.join(jars));
            final PhSuggestions suggestions = new PhSuggestions();
            MatcherAssert.assertThat(
                "Default suggestions must be built from classpath JAR entries",
                Arrays.asList(
                    suggestions.suggestions(String.format("Φ.%s.prnter", first), 5).get(0),
                    suggestions.suggestions(String.format("Φ.%s.loger", second), 5).get(0)
                ),
                Matchers.contains(
                    String.format("%s.printer", first),
                    String.format("%s.logger", second)
                )
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
     * Java classpath system property.
     * @return Property name
     */
    private static String classpath() {
        return "java.class.path";
    }

    /**
     * Create dependency JAR with EO-style object resource.
     * @param dir Directory
     * @param name JAR file name
     * @param object Object part
     * @return JAR path
     * @throws IOException If JAR can't be written
     */
    private static Path dependency(
        final Path dir,
        final String name,
        final String object
    ) throws IOException {
        final Path jar = dir.resolve(name);
        try (JarOutputStream output = new JarOutputStream(Files.newOutputStream(jar))) {
            output.putNextEntry(
                new JarEntry(
                    String.format("org/eolang/EO%s.class", object.replace(".", "/EO"))
                )
            );
            output.closeEntry();
        }
        return jar;
    }

    /**
     * Join classpath entries.
     * @param entries Entries
     * @return Classpath
     */
    private static String join(final List<Path> entries) {
        return entries.stream()
            .map(Path::toString)
            .collect(Collectors.joining(File.pathSeparator));
    }

    /**
     * Unique EO package part based on JUnit temporary directory.
     * @param temp Temporary directory
     * @param prefix Prefix
     * @return Unique package part
     */
    private static String unique(final Path temp, final String prefix) {
        return String.format(
            "%s%s",
            prefix,
            temp.getFileName().toString().replaceAll("[^A-Za-z0-9]", "")
        );
    }

    /**
     * Find fixture dependency JAR.
     * @return Fixture entry
     */
    private static String fixture() {
        final List<Path> entries = Arrays.asList(
            Paths.get("..", PhSuggestionsTest.marker(), "target", PhSuggestionsTest.jar()),
            Paths.get(PhSuggestionsTest.marker(), "target", PhSuggestionsTest.jar()),
            Paths.get(
                System.getProperty("user.home"),
                ".m2",
                "repository",
                "org",
                "eolang",
                PhSuggestionsTest.marker(),
                PhSuggestionsTest.version(),
                PhSuggestionsTest.jar()
            )
        );
        return entries.stream()
            .filter(Files::isRegularFile)
            .map(Path::toAbsolutePath)
            .map(Path::normalize)
            .map(Path::toString)
            .findFirst()
            .orElseThrow(() -> PhSuggestionsTest.absent(entries));
    }

    /**
     * Absent fixture error.
     * @param entries Entries
     * @return Error
     */
    private static IllegalStateException absent(final List<Path> entries) {
        return new IllegalStateException(
            String.format(
                "Fixture dependency JAR '%s' was not found among %s",
                PhSuggestionsTest.jar(),
                entries
            )
        );
    }

    /**
     * Fixture artifact name.
     * @return Artifact name
     */
    private static String jar() {
        return String.format("%s-%s.jar", PhSuggestionsTest.marker(), PhSuggestionsTest.version());
    }

    /**
     * Fixture artifact marker.
     * @return Artifact marker
     */
    private static String marker() {
        return "eo-suggestions-fixture";
    }

    /**
     * Fixture version.
     * @return Version
     */
    private static String version() {
        return System.getProperty("eo.version", "1.0-SNAPSHOT");
    }

    /**
     * Restore classpath property.
     * @param origin Original value
     */
    private static void restore(final String origin) {
        if (origin == null) {
            System.clearProperty(PhSuggestionsTest.classpath());
        } else {
            System.setProperty(PhSuggestionsTest.classpath(), origin);
        }
    }
}
