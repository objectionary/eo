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
            new StatLayout(true, false, true).stat("/tmp/f"),
            Matchers.instanceOf(MacFileStat.class)
        );
    }

    @Test
    void picksTheWideArmLayout() {
        MatcherAssert.assertThat(
            "a 64-bit ARM must get the aarch64 struct stat, but it didnt",
            new StatLayout(false, true, true).stat("/tmp/f"),
            Matchers.instanceOf(LinuxArmFileStat.class)
        );
    }

    @Test
    void picksTheLinuxLayout() {
        MatcherAssert.assertThat(
            "a non-ARM Linux must get the x86-64 struct stat, but it didnt",
            new StatLayout(false, false, true).stat("/tmp/f"),
            Matchers.instanceOf(LinuxFileStat.class)
        );
    }

    @Test
    void refusesToReadThirtyTwoBitArm() {
        MatcherAssert.assertThat(
            "32-bit ARM must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new StatLayout(false, true, false).stat("/tmp/f"),
                "32-bit ARM was expected to fail with ExFailure"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("32-bit ARM"),
                Matchers.containsString("/tmp/f")
            )
        );
    }
}
