/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
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
}
