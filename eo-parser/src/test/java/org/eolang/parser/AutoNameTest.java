/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Random;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link AutoName}.
 * @since 0.58.1
 */
final class AutoNameTest {

    @Test
    void generatesAutoNameForLineAndIndent() {
        MatcherAssert.assertThat(
            "auto name is not the expected line-indent placeholder",
            new AutoName(42, 13).asString(),
            Matchers.equalTo("a🌵42-13")
        );
    }

    @Test
    void generatesAutoNameForZeroIndent() {
        final long seed = 7163L;
        final int line = new Random(seed).nextInt(1000) + 1;
        MatcherAssert.assertThat(
            String.format("auto name is not zero-indent placeholder, seed %d", seed),
            new AutoName(line, 0).asString(),
            Matchers.equalTo(String.format("a🌵%d-0", line))
        );
    }

    @Test
    void generatesAutoNameForLargeLineAndIndent() {
        final long seed = 20260820L;
        final Random random = new Random(seed);
        final int line = random.nextInt(100_000) + 100_000;
        final int indent = random.nextInt(1000) + 100;
        MatcherAssert.assertThat(
            String.format("auto name is not the expected placeholder, seed %d", seed),
            new AutoName(line, indent).asString(),
            Matchers.equalTo(String.format("a🌵%d-%d", line, indent))
        );
    }
}
