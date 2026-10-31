/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.lang.management.ManagementFactory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link EOwin32$EOgetpid}.
 *
 * @since 0.77.0
 */
final class EOwin32EOgetpidTest {

    @Test
    @DisabledOnOs({OS.LINUX, OS.MAC})
    void reportsIdentifierOfRunningProcess() {
        MatcherAssert.assertThat(
            "win32.getpid did not report the identifier of the process it runs in",
            new Dataized(new EOwin32$EOgetpid()).asNumber().intValue(),
            Matchers.equalTo(
                Integer.parseInt(
                    ManagementFactory.getRuntimeMXBean().getName().split("@", -1)[0]
                )
            )
        );
    }
}
