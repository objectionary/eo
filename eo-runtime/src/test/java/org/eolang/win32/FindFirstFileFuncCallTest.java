/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link FindFirstFileFuncCall}.
 * @since 0.76.0
 */
final class FindFirstFileFuncCallTest {

    @Test
    void refusesAPatternWithNul() {
        MatcherAssert.assertThat(
            "the 'pattern' argument of FindFirstFile carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new FindFirstFileFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi(String.join(String.valueOf((char) 0), "one", "two"))
                ),
                "a 'pattern' argument of FindFirstFile with a NUL was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'pattern' argument of FindFirstFile"),
                Matchers.containsString("NUL")
            )
        );
    }
}
