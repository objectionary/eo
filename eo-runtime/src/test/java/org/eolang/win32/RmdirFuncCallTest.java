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
 * Test case for {@link RmdirFuncCall}.
 *
 * @since 0.57.0
 */
final class RmdirFuncCallTest {

    @Test
    void refusesPathWithNul() {
        MatcherAssert.assertThat(
            "the 'path' argument of rmdir carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new RmdirFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi(String.join(String.valueOf((char) 0), "one", "two"))
                ),
                "a 'path' argument of rmdir with a NUL was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'path' argument of rmdir"),
                Matchers.containsString("NUL")
            )
        );
    }
}
