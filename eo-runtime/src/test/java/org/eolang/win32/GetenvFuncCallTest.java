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
 * Test case for {@link GetenvFuncCall}.
 *
 * @since 0.57.0
 */
final class GetenvFuncCallTest {

    @Test
    void refusesNameWithNul() {
        MatcherAssert.assertThat(
            "the 'name' argument of getenv carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new GetenvFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi(String.join(String.valueOf((char) 0), "one", "two"))
                ),
                "a 'name' argument of getenv with a NUL was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'name' argument of getenv"),
                Matchers.containsString("NUL")
            )
        );
    }
}
