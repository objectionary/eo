/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;
import org.eolang.lints.Severity;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Linting}.
 *
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
}
