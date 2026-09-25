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
 *
 * @since 0.74.0
 */
final class StatLayoutTest {

    @Test
    void picksTheMacLayoutWhateverTheArchitecture() {
        MatcherAssert.assertThat(
            "macOS must get its own struct stat, but it didnt",
            new StatLayout("aarch64", true).stat("/tmp/one"),
            Matchers.instanceOf(MacFileStat.class)
        );
    }

    @Test
    void picksTheWideArmLayout() {
        MatcherAssert.assertThat(
            "a 64-bit ARM must get the aarch64 struct stat, but it didnt",
            new StatLayout("aarch64", false).stat("/tmp/two"),
            Matchers.instanceOf(LinuxArmFileStat.class)
        );
    }

    @Test
    void picksTheIntelLayout() {
        MatcherAssert.assertThat(
            "an x86-64 must get the x86-64 struct stat, but it didnt",
            new StatLayout("x86-64", false).stat("/tmp/three"),
            Matchers.instanceOf(LinuxFileStat.class)
        );
    }

    @Test
    void refusesNarrowArmInsteadOfReadingTheWideLayout() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new StatLayout("arm", false).stat("/tmp/four"),
            "a 32-bit ARM must be refused, since it doesnt share the aarch64 struct stat"
        );
    }

    @Test
    void refusesRiscvInsteadOfReadingTheIntelLayout() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new StatLayout("riscv64", false).stat("/tmp/five"),
            "a RISC-V must be refused, since it doesnt share the x86-64 struct stat"
        );
    }

    @Test
    void namesTheArchitectureItCannotRead() {
        MatcherAssert.assertThat(
            "the refusal must name the architecture nobody mapped, but it didnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new StatLayout("loongarch64", false).stat("/tmp/six")
            ).getMessage(),
            Matchers.containsString("loongarch64")
        );
    }
}
