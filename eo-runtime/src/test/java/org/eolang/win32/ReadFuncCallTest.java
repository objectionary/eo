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
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link ReadFuncCall}.
 *
 * @since 0.74.0
 */
final class ReadFuncCallTest {

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void rejectsSizeNoArrayCanHoldOnWindowsRead() {
        MatcherAssert.assertThat(
            "the 'size' argument of read must be refused by name, the way posix refuses it",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new ReadFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi(0), new Data.ToPhi(Integer.MAX_VALUE)
                ),
                "a 'size' argument of read beyond any array was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'size' argument of read"),
                Matchers.containsString("Can't allocate")
            )
        );
    }
}
