/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.regex.PatternSyntaxException;
import org.cactoos.list.ListOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link GlobPatterns}.
 *
 * @since 0.62.3
 */
final class GlobPatternsTest {

    @Test
    void compilesToPatterns() {
        MatcherAssert.assertThat(
            "Globs must be compiled to patterns",
            new GlobPatterns(
                new ListOf<>("config-[!a-z].yaml", "{src,test}/{main,test}/**/*.java")
            ).value(),
            Matchers.hasSize(2)
        );
    }

    @Test
    void failsOnInvalidGlobs() {
        Assertions.assertThrows(
            PatternSyntaxException.class,
            () -> new GlobPatterns(new ListOf<>("{foo")).value(),
            "Exception must be thrown for invalid glob pattern"
        );
    }
}
