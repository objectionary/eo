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
 * Test case for {@link AccessFuncCall}.
 *
 * @since 0.57.0
 */
final class AccessFuncCallTest {

    @Test
    void refusesPathWithNul() {
        MatcherAssert.assertThat(
            "the 'path' argument of access carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new AccessFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi(String.join(String.valueOf((char) 0), "one", "two")),
                    new Data.ToPhi(0L)
                ),
                "a 'path' argument of access with a NUL was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'path' argument of access"),
                Matchers.containsString("NUL")
            )
        );
    }

    @Test
    void refusesFractionalMode() {
        MatcherAssert.assertThat(
            "the 'mode' argument of access must be refused by name when it is not an integer, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new AccessFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi("one"),
                    new Data.ToPhi(3.9)
                ),
                "a fractional 'mode' argument of access was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'mode' argument of access"),
                Matchers.containsString("integer")
            )
        );
    }
}
