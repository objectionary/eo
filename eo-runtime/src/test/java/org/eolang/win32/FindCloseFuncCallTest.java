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
 * Test case for {@link FindCloseFuncCall}.
 * @since 0.76.0
 */
final class FindCloseFuncCallTest {

    @Test
    void refusesAHandleNobodyOpened() {
        MatcherAssert.assertThat(
            "closing a search twice must be refused, since the second close would free a freed handle",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new FindCloseFuncCall(Phi.Φ.take("win32").copy()).make(
                    new Data.ToPhi(-1)
                ),
                "closing a search that was never started was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.containsString("'search' argument of FindClose")
        );
    }
}
