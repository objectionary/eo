/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link EOwin32$EOfind_close}.
 *
 * @since 0.77.0
 */
final class EOwin32EOfind_closeTest {

    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC})
    void refusesAHandleNobodyOpened() {
        MatcherAssert.assertThat(
            "closing a search twice must be refused, since the second close would free a freed handle",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOwin32$EOfind_close(),
                        new Bind("search", new Data.ToPhi(-1L))
                    )
                ).take(),
                "closing a search that was never started was expected to fail"
            ).getMessage(),
            Matchers.containsString("'search' attribute")
        );
    }
}
