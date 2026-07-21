/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

import java.lang.management.ManagementFactory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link EOposix}.
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOposixTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void invokesGetpidCorrectly() {
        MatcherAssert.assertThat(
            "The \"getpid\" system call was expected to work correctly",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        Phi.Φ.take("posix").copy(),
                        "name",
                        new Data.ToPhi("getpid")
                    ),
                    "args",
                    Phi.Φ.take("tuple").take("empty")
                ).take("code")
            ).asNumber().intValue(),
            Matchers.equalTo(
                Integer.parseInt(
                    ManagementFactory.getRuntimeMXBean()
                        .getName().split("@")[0]
                )
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void reportsReasonWhenOpenFails() {
        MatcherAssert.assertThat(
            "Failed \"open\" should carry the OS error reason in its output",
            new Dataized(
                new PhApplication(
                    new PhApplication(
                        Phi.Φ.take("posix").copy(),
                        "name",
                        new Data.ToPhi("open")
                    ),
                    "args",
                    new Data.ToPhi(
                        new Phi[]{
                            new Data.ToPhi("/eo-5403-absent-directory/file.txt"),
                            new Data.ToPhi(0),
                            new Data.ToPhi(0),
                        }
                    )
                ).take("output")
            ).asString(),
            Matchers.containsString("No such file")
        );
    }
}
