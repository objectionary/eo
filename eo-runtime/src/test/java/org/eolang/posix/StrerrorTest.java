/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.Native;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link Strerror}.
 *
 * <p>The number 17 below is {@code EEXIST}, the error a failing exclusive
 * creation leaves behind on every platform we support.</p>
 *
 * <p>The tests that go through the real library are off on Windows, where
 * there is no {@code libc} to load and {@link CStdLib} cannot be built at
 * all.</p>
 *
 * @since 0.75
 */
final class StrerrorTest {

    @Test
    void translatesTheCode() {
        MatcherAssert.assertThat(
            "the message of the code asked for must come back",
            new Strerror(code -> String.format("error #%d", code), 17).it(),
            Matchers.equalTo("error #17")
        );
    }

    @Test
    void keepsTheLastErrorWhenTheLookupSpoilsIt() {
        Native.setLastError(17);
        new Strerror(
            code -> {
                Native.setLastError(2);
                return "spoiled";
            },
            17
        ).it();
        MatcherAssert.assertThat(
            "the error of the failing call must survive a lookup that spoils it",
            Native.getLastError(),
            Matchers.equalTo(17)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void namesTheErrorThroughTheRealLibrary() {
        MatcherAssert.assertThat(
            "libc must name the error",
            new Strerror(17).it(),
            Matchers.not(Matchers.emptyString())
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void keepsTheLastErrorThroughTheRealLibrary() {
        Native.setLastError(17);
        new Strerror(17).it();
        MatcherAssert.assertThat(
            "the error of the failing call must survive the lookup in libc",
            Native.getLastError(),
            Matchers.equalTo(17)
        );
    }
}
