/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Cstring}.
 * @since 0.57.0
 */
final class CstringTest {

    @Test
    void takesOrdinaryText() {
        MatcherAssert.assertThat(
            "text without a NUL must pass through unchanged, but it didnt",
            new Cstring(
                new Expect<>("the 'path' argument", () -> new Data.ToPhi("/tmp/file"))
            ).it(),
            Matchers.equalTo("/tmp/file")
        );
    }

    @Test
    void namesTheConversionFailure() {
        MatcherAssert.assertThat(
            "an argument that is not a text must be named as such, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Cstring(
                    new Expect<>("the 'name' argument", () -> new PhTerminator("boom"))
                ).it(),
                "a non-text argument was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("the 'name' argument must be a text"),
                Matchers.not(Matchers.containsString("NUL"))
            )
        );
    }

    @Test
    void refusesTextWithNul() {
        MatcherAssert.assertThat(
            "text carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Cstring(
                    new Expect<>(
                        "the 'name' argument",
                        () -> new Data.ToPhi(String.join(String.valueOf((char) 0), "PATH", "nope"))
                    )
                ).it(),
                "text with a NUL was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("the 'name' argument"),
                Matchers.containsString("NUL")
            )
        );
    }
}
