/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Registry}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
@DisabledOnOs(OS.WINDOWS)
final class RegistryTest {

    @Test
    void servesEveryLambdaFromOneEntry(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the operations, the fork and the boxes must be served by one entry, but they arent",
            Files.readString(RegistryTest.saved(temp), StandardCharsets.UTF_8),
            Matchers.allOf(
                Matchers.containsString("L_number_plus|"),
                Matchers.containsString("|L_fork|L_box_\\\\d+\":{\"rt\":\"exec\""),
                Matchers.containsString("\"serve\":true")
            )
        );
    }

    @Test
    void writesExecutableLauncherNamingTheTables(@Mktmp final Path temp) throws IOException {
        RegistryTest.saved(temp);
        MatcherAssert.assertThat(
            "the launcher must hand the tables to the engine, but it doesnt",
            Files.readString(temp.resolve("engine"), StandardCharsets.UTF_8),
            Matchers.allOf(
                Matchers.startsWith("#!/bin/sh"),
                Matchers.containsString(String.format("SYMBOLS='%s'", temp.resolve("s.tsv"))),
                Matchers.containsString("org.eolang.atoms.Engine")
            )
        );
    }

    @Test
    void namesTheFileTheTripsAreCountedIn(@Mktmp final Path temp) throws IOException {
        RegistryTest.saved(temp);
        MatcherAssert.assertThat(
            "the launcher must tell the engine where to count its trips, but it doesnt",
            Files.readString(temp.resolve("engine"), StandardCharsets.UTF_8),
            Matchers.containsString(String.format("TRIPS='%s'", temp.resolve("t.txt")))
        );
    }

    @Test
    void makesLauncherExecutable(@Mktmp final Path temp) throws IOException {
        RegistryTest.saved(temp);
        MatcherAssert.assertThat(
            "the launcher must be executable, but it isnt",
            Files.getPosixFilePermissions(temp.resolve("engine")),
            Matchers.hasItem(PosixFilePermission.OWNER_EXECUTE)
        );
    }

    private static Path saved(final Path temp) throws IOException {
        return new Registry(
            temp, temp.resolve("s.tsv"), temp.resolve("b.tsv"), temp.resolve("t.txt")
        ).saved();
    }
}
