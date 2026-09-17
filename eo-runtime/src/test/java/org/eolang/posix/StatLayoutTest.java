/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.ExFailure;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link StatLayout}.
 * @since 0.74.0
 */
final class StatLayoutTest {

    @Test
    void picksTheMacLayout() {
        MatcherAssert.assertThat(
            "macOS must get its own struct stat, but it didnt",
            new StatLayout(true, false, false).stat("/tmp/f"),
            Matchers.instanceOf(MacFileStat.class)
        );
    }

    @Test
    void picksTheArmLayout() {
        MatcherAssert.assertThat(
            "ARM must get the aarch64 struct stat, but it didnt",
            new StatLayout(false, true, false).stat("/tmp/f"),
            Matchers.instanceOf(LinuxArmFileStat.class)
        );
    }

    @Test
    void picksTheIntelLayout() {
        MatcherAssert.assertThat(
            "an x86 Linux must get the x86-64 struct stat, but it didnt",
            new StatLayout(false, false, true).stat("/tmp/f"),
            Matchers.instanceOf(LinuxFileStat.class)
        );
    }

    @Test
    void refusesAnArchitectureItDoesNotKnow() {
        MatcherAssert.assertThat(
            "an unmapped architecture must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new StatLayout(false, false, false).stat("/tmp/f"),
                "an unmapped architecture was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("/tmp/f"),
                Matchers.containsString("architecture")
            )
        );
    }
}
