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
 * Test case for {@link RenameFuncCall}.
 *
 * @since 0.57.0
 */
final class RenameFuncCallTest {

    @Test
    void refusesSourceWithNul() {
        MatcherAssert.assertThat(
            "the 'from' argument carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new RenameFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi(String.join(String.valueOf((char) 0), "one", "two")),
                    new Data.ToPhi("plain")
                ),
                "a 'from' argument with a NUL was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'from' argument of rename"),
                Matchers.containsString("NUL")
            )
        );
    }

    @Test
    void refusesTargetWithNul() {
        MatcherAssert.assertThat(
            "the 'to' argument carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new RenameFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi("plain"),
                    new Data.ToPhi(String.join(String.valueOf((char) 0), "one", "two"))
                ),
                "a 'to' argument with a NUL was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'to' argument of rename"),
                Matchers.containsString("NUL")
            )
        );
    }
}
