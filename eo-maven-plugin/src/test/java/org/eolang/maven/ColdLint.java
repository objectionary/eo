/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Files;

/**
 * Child JVM entry point that runs a single cold linting pass.
 *
 * <p>The shared lint stylesheets compile once per JVM, so the race in #5209 has
 * exactly one opportunity per process: the first parallel pass. This entry point
 * lints many programs in one fresh JVM and propagates any failure as a non-zero
 * exit code, letting {@link LintingTest} fork many of these to observe the race
 * reliably.</p>
 *
 * @since 0.57.0
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
public final class ColdLint {

    /**
     * Not for instantiation.
     */
    private ColdLint() {
    }

    /**
     * Run one cold lint pass; a race surfaces as a thrown exception.
     * @param args Ignored
     * @throws Exception If linting fails (race reproduced)
     */
    public static void main(final String... args) throws Exception {
        final FakeMaven maven = new FakeMaven(Files.createTempDirectory("coldlint"))
            .with("cacheEnabled", false);
        for (int idx = 0; idx < Runtime.getRuntime().availableProcessors() * 4; ++idx) {
            maven.withProgram(
                String.join(
                    System.lineSeparator(),
                    "+home https://www.eolang.org",
                    String.format("+package foo.p%d", idx),
                    "+version 0.0.0",
                    "+unlint object-has-data",
                    "",
                    "# No comments.",
                    "[x] > main",
                    String.format("  (\"Hi %d\" x).length > @", idx)
                ),
                String.format("foo.p%d.main", idx),
                String.format("foo/p%d/main.eo", idx)
            );
        }
        maven.execute(new FakeMaven.Lint());
    }
}
