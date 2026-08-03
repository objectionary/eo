/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.eolang.lints.Severity;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Linting}.
 * @since 0.31.0
 */
final class LintingTest {

    @Test
    void summarizesAllThreeSeveritiesTogether() {
        final Map<Severity, Integer> counts = new EnumMap<>(Severity.class);
        counts.put(Severity.CRITICAL, 2);
        counts.put(Severity.ERROR, 4);
        counts.put(Severity.WARNING, 7);
        MatcherAssert.assertThat(
            "the summary must mention the error count, not just critical and warnings",
            Linting.summary(counts),
            Matchers.equalTo("2 critical errors, 4 errors, and 7 warnings")
        );
    }

    @Test
    void skipsLintingWhenFlagIsSet(@TempDir final Path temp) {
        final TjsForeign tojos = new TjsForeign();
        Assertions.assertDoesNotThrow(
            () -> new Linting(
                tojos,
                tojos,
                temp,
                temp,
                false,
                "0.0.0",
                Collections.emptyList(),
                Collections.emptyList(),
                false,
                false,
                false,
                true
            ).exec(),
            "Linting must be fully skipped when skipLinting is TRUE"
        );
    }

    @Test
    void changesWpaCacheKeyWhenCompileScopeXmirChanges(@TempDir final Path temp)
        throws IOException {
        final Path cache = temp.resolve("cache");
        final Path target = temp.resolve("target");
        final Path dep = temp.resolve("dep.xmir");
        Files.writeString(dep, "<object><o line=\"1\" name=\"one\"/></object>");
        final TjsForeign compile = new TjsForeign();
        compile.add("dep").withXmir(dep).withScope("compile");
        LintingTest.lintAsPackage(compile, target, cache);
        final Set<String> first = LintingTest.wpaCacheEntries(cache);
        Files.writeString(dep, "<object><o line=\"1\" name=\"two\"/></object>");
        LintingTest.lintAsPackage(compile, target, cache);
        MatcherAssert.assertThat(
            "changing a compile-scope XMIR's content must produce a new WPA cache entry, not reuse the stale one",
            LintingTest.wpaCacheEntries(cache),
            Matchers.not(Matchers.equalTo(first))
        );
    }

    /**
     * Run linting as a package, with a fresh, empty tojos set of the
     * project's own sources, over the given compile-scope tojos.
     * @param compile Compile-scope tojos to analyze
     * @param target Target directory
     * @param cache Cache directory
     * @throws IOException If linting fails
     */
    private static void lintAsPackage(
        final TjsForeign compile, final Path target, final Path cache
    ) throws IOException {
        new Linting(
            new TjsForeign(),
            compile,
            target,
            cache,
            LintingTest.cacheEnabled(),
            "0.0.0",
            Collections.emptyList(),
            Collections.emptyList(),
            LintingTest.skipExperimentalLints(),
            LintingTest.failOnWarning(),
            LintingTest.asPackage(),
            LintingTest.skipLinting()
        ).exec();
    }

    /**
     * Whether caching is enabled, for {@link #lintAsPackage(TjsForeign, Path, Path)}.
     * @return True
     */
    private static boolean cacheEnabled() {
        return true;
    }

    /**
     * Whether to skip experimental lints, for
     * {@link #lintAsPackage(TjsForeign, Path, Path)}.
     * @return False
     */
    private static boolean skipExperimentalLints() {
        return false;
    }

    /**
     * Whether to fail on warnings, for
     * {@link #lintAsPackage(TjsForeign, Path, Path)}.
     * @return False
     */
    private static boolean failOnWarning() {
        return false;
    }

    /**
     * Whether to lint all sources as a package (WPA), for
     * {@link #lintAsPackage(TjsForeign, Path, Path)}.
     * @return True
     */
    private static boolean asPackage() {
        return true;
    }

    /**
     * Whether to skip linting entirely, for
     * {@link #lintAsPackage(TjsForeign, Path, Path)}.
     * @return False
     */
    private static boolean skipLinting() {
        return false;
    }

    /**
     * The set of WPA cache entry directory names under the given cache
     * directory.
     * @param cache Cache directory
     * @return Directory names, one per distinct cache key seen so far
     * @throws IOException If the directory can't be listed
     */
    private static Set<String> wpaCacheEntries(final Path cache) throws IOException {
        final Path wpa = cache.resolve(Linting.CACHE);
        final Set<String> entries;
        if (Files.exists(wpa)) {
            try (Stream<Path> list = Files.list(wpa)) {
                entries = list.map(p -> p.getFileName().toString()).collect(Collectors.toSet());
            }
        } else {
            entries = Collections.emptySet();
        }
        return entries;
    }
}
