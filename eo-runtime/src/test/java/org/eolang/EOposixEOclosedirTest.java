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
 * Test case for {@link EOposix$EOclosedir}.
 *
 * @since 0.77.0
 */
final class EOposixEOclosedirTest {

    @Test
    void refusesAHandleNobodyOpened() {
        MatcherAssert.assertThat(
            "a second close of a stream must be refused, since it would free a freed pointer",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(new EOposix$EOclosedir(), "dirp", new Data.ToPhi(-1))
                        .take("code")
                ).take(),
                "closing a stream that was never opened was expected to fail"
            ).getMessage(),
            Matchers.containsString("'dirp' attribute")
        );
    }
}
