/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link EOwin32$EOrecv}.
 *
 * @since 0.77.0
 */
final class EOwin32EOrecvTest {

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void reportsFailureOnSocketNobodyOpened() {
        MatcherAssert.assertThat(
            "a failed recv must not report anything but -1, nor crash",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        new PhApplication(
                            new EOwin32$EOrecv(), "descriptor", new Data.ToPhi(-1)
                        ),
                        "size",
                        new Data.ToPhi(16)
                    ),
                    "flags",
                    new Data.ToPhi(0)
                ).take("code")
            ).asNumber().intValue(),
            Matchers.equalTo(-1)
        );
    }
}
