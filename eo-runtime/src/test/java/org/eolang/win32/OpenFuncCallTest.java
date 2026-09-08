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
 * Test case for {@link OpenFuncCall}.
 *
 * @since 0.57.0
 */
final class OpenFuncCallTest {

    @Test
    void refusesPathWithNul() {
        MatcherAssert.assertThat(
            "the 'path' argument of open carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new OpenFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi(String.join(String.valueOf((char) 0), "one", "two")),
                    new Data.ToPhi(0L),
                    new Data.ToPhi(0L)
                ),
                "a 'path' argument of open with a NUL was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'path' argument of open"),
                Matchers.containsString("NUL")
            )
        );
    }
}
