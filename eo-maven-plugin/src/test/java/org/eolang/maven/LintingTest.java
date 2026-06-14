/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Linting}.
 * @since 0.31.0
 */
final class LintingTest {

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
                temp,
                true
            ).exec(),
            "Linting must be fully skipped when skipLinting is TRUE"
        );
    }

    @Test
    void lintsManyProgramsConcurrentlyWithoutStylesheetRace() throws Exception {
        final int forks = 24;
        final String java = Paths.get(
            System.getProperty("java.home"), "bin", "java"
        ).toString();
        final String classpath = System.getProperty("java.class.path");
        final Collection<Process> children = new ArrayList<>(forks);
        for (int fork = 0; fork < forks; ++fork) {
            children.add(
                new ProcessBuilder(java, "-cp", classpath, "org.eolang.maven.ColdLint")
                    .redirectErrorStream(true)
                    .redirectOutput(ProcessBuilder.Redirect.DISCARD)
                    .start()
            );
        }
        int raced = 0;
        for (final Process child : children) {
            if (child.waitFor() != 0) {
                raced += 1;
            }
        }
        Assertions.assertEquals(
            0,
            raced,
            String.format(
                "Lint stylesheets must compile once before parallel linting, but %d of %d cold JVMs raced on first-time compilation (see #5209)",
                raced, forks
            )
        );
    }
}
