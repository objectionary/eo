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
 * Test case for {@link EOposix$EOread}.
 *
 * @since 0.77.0
 */
final class EOposixEOreadTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void reportsFailureOnDescriptorNobodyOpened() {
        MatcherAssert.assertThat(
            "a read of a descriptor nobody opened must not report anything but -1",
            new Dataized(
                new PhApplication(
                    new PhApplication(new EOposix$EOread(), "descriptor", new Data.ToPhi(-3)),
                    "size",
                    new Data.ToPhi(27)
                ).take("code")
            ).asNumber().intValue(),
            Matchers.equalTo(-1)
        );
    }
}
