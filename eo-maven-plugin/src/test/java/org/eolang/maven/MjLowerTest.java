/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import org.cactoos.list.ListOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjLower}.
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class MjLowerTest {

    @Test
    void doesNothingWhenDisabled(@Mktmp final Path temp) throws IOException {
        final Path home = temp.resolve("target/eo/7-lower");
        new FakeMaven(temp)
            .with("home", home.toFile())
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "a disabled goal must leave no folder behind, but it made one",
            home.toFile(),
            Matchers.not(FileMatchers.anExistingDirectory())
        );
    }

    @Test
    void failsWhenTheBinaryIsMissing(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the failure must name the version the lowering is pinned to, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new FakeMaven(temp)
                    .with("lowering", true)
                    .with("binary", temp.resolve("absent").toString())
                    .with("home", temp.resolve("target/eo/7-lower").toFile())
                    .execute(MjLower.class),
                "a binary that is not there must fail the build"
            ).getCause().getCause().getMessage(),
            Matchers.containsString("0.0.136")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void createsItsFolderWhenPhinoReportsThePinnedVersion(@Mktmp final Path temp)
        throws IOException {
        final Path binary = temp.resolve("phino");
        Files.write(
            binary, new ListOf<>("#!/bin/sh", "echo 0.0.136")
        );
        Files.setPosixFilePermissions(
            binary, PosixFilePermissions.fromString("rwxr-xr-x")
        );
        final Path home = temp.resolve("target/eo/7-lower");
        new FakeMaven(temp)
            .with("lowering", true)
            .with("binary", binary.toString())
            .with("tables", MjLowerTest.tables(temp).toFile())
            .with("home", home.toFile())
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "the goal must make the folder it was given, but it didnt",
            home.toFile(),
            FileMatchers.anExistingDirectory()
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void plantsTheEntriesOfTheProgramItCompiled(@Mktmp final Path temp) throws IOException {
        final Path binary = temp.resolve("phino");
        Files.write(
            binary, new ListOf<>("#!/bin/sh", "echo 0.0.136")
        );
        Files.setPosixFilePermissions(
            binary, PosixFilePermissions.fromString("rwxr-xr-x")
        );
        final Path home = temp.resolve("target/eo/7-lower");
        new FakeMaven(temp)
            .withProgram(String.format("[a b] > gap%n  a.plus b > @%n"))
            .execute(MjParse.class)
            .with("lowering", true)
            .with("binary", binary.toString())
            .with("tables", MjLowerTest.tables(temp).toFile())
            .with("home", home.toFile())
            .execute(MjLower.class);
        MatcherAssert.assertThat(
            "the goal must plant the formations of the program it compiled, but it didnt",
            new XMLDocument(home.resolve("entries.xmir")).xpath("//o[@name='e1']/@base"),
            Matchers.contains("Φ.l🌵.mark")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void namesTheTablesItCannotFind(@Mktmp final Path temp) throws IOException {
        final Path binary = temp.resolve("phino");
        Files.write(
            binary, new ListOf<>("#!/bin/sh", "echo 0.0.136")
        );
        Files.setPosixFilePermissions(
            binary, PosixFilePermissions.fromString("rwxr-xr-x")
        );
        final Path absent = temp.resolve("nowhere");
        MatcherAssert.assertThat(
            "the failure must name the directory the tables are missing from, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new FakeMaven(temp)
                    .with("lowering", true)
                    .with("binary", binary.toString())
                    .with("tables", absent.toFile())
                    .with("home", temp.resolve("target/eo/7-lower").toFile())
                    .execute(MjLower.class),
                "tables that are not there must fail the build"
            ).getCause().getCause().getMessage(),
            Matchers.containsString(absent.toString())
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void failsWhenTheBinaryReportsAnotherVersion(@Mktmp final Path temp) throws IOException {
        final Path binary = temp.resolve("phino");
        Files.write(
            binary, new ListOf<>("#!/bin/sh", "echo 0.0.1")
        );
        Files.setPosixFilePermissions(
            binary, PosixFilePermissions.fromString("rwxr-xr-x")
        );
        MatcherAssert.assertThat(
            "the failure must name both the version found and the one pinned, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new FakeMaven(temp)
                    .with("lowering", true)
                    .with("binary", binary.toString())
                    .with("home", temp.resolve("target/eo/7-lower").toFile())
                    .execute(MjLower.class),
                "a binary of another version must fail the build"
            ).getCause().getCause().getMessage(),
            Matchers.stringContainsInOrder("0.0.1", "0.0.136")
        );
    }

    private static Path tables(final Path temp) throws IOException {
        final Path made = Files.createDirectories(temp.resolve("tables"));
        Files.write(
            made.resolve("provides.xml"), "<provides/>".getBytes(StandardCharsets.UTF_8)
        );
        return made;
    }
}
