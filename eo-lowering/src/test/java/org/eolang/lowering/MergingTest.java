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
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Collections;
import org.cactoos.list.ListOf;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Merging}.
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class MergingTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void handsEverySourceAndThenTheEntriesToOneCall(@Mktmp final Path temp)
        throws IOException {
        final Path home = MergingTest.planted(temp);
        final Path source = MergingTest.xmir(temp, "gap");
        new Merging(
            Collections.singletonList(source),
            home,
            MergingTest.phino(
                temp,
                String.join(
                    " ",
                    "while [ $# -gt 0 ]; do case $1 in --target) t=$2; shift;;",
                    "*) a=\"$a $1\";; esac; shift; done; echo \"$a\" > \"$t\""
                )
            )
        ).exec();
        MatcherAssert.assertThat(
            "the one call must take every source before the entries, but it doesnt",
            new String(Files.readAllBytes(home.resolve("world.phi")), StandardCharsets.UTF_8),
            Matchers.stringContainsInOrder(
                "merge", "--input=xmir", source.toString(), "entries.xmir"
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void printsTheWorldSweet(@Mktmp final Path temp) throws IOException {
        final Path home = MergingTest.planted(temp);
        new Merging(
            Collections.singletonList(MergingTest.xmir(temp, "gap")),
            home,
            MergingTest.phino(
                temp,
                String.join(
                    " ",
                    "while [ $# -gt 0 ]; do case $1 in --target) t=$2; shift;;",
                    "*) a=\"$a $1\";; esac; shift; done; echo \"$a\" > \"$t\""
                )
            )
        ).exec();
        MatcherAssert.assertThat(
            "the world must be printed with syntax sugar, but it isnt",
            new String(Files.readAllBytes(home.resolve("world.phi")), StandardCharsets.UTF_8),
            Matchers.containsString("--sweet")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void quotesWhatTheBinaryPrintedWhenItRefusesTheWorld(@Mktmp final Path temp)
        throws IOException {
        final Path source = MergingTest.xmir(temp, "gap");
        final Path home = MergingTest.planted(temp);
        final Phino phino = MergingTest.phino(
            temp, "echo 'no world for this one' >&2; exit 2"
        );
        MatcherAssert.assertThat(
            "the failure must quote what the binary printed, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Merging(Collections.singletonList(source), home, phino).exec(),
                "a binary that exits with an error must fail the merging"
            ).getMessage(),
            Matchers.containsString("no world for this one")
        );
    }

    @Test
    void failsNamingTheEntriesItCannotFind(@Mktmp final Path temp) {
        final Path home = temp.resolve("lower");
        MatcherAssert.assertThat(
            "the failure must name the entries that are missing, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Merging(new ListOf<>(), home, new Phino("absent")).exec(),
                "a home without entries must fail the merging"
            ).getMessage(),
            Matchers.containsString(home.resolve("entries.xmir").toString())
        );
    }

    @Test
    void mergesAWorldThePinnedPhinoReads(@Mktmp final Path temp) throws IOException {
        final Phino phino = new Phino("phino");
        Assumptions.assumeTrue(
            MergingTest.pinned(phino),
            "the pinned phino is not on this machine, so the world cannot be merged here"
        );
        final Path source = Files.write(
            temp.resolve("gap.xmir"),
            new EoSyntax(String.format("[a b] > gap%n  a.plus b > @%n")).parsed()
                .toString().getBytes(StandardCharsets.UTF_8)
        );
        final Path home = temp.resolve("lower");
        final Path tables = Files.createDirectories(temp.resolve("tables"));
        Files.write(tables.resolve("provides.xml"), "<provides/>".getBytes(StandardCharsets.UTF_8));
        new Planting(Collections.singletonList(source), tables, home).exec();
        new Merging(Collections.singletonList(source), home, phino).exec();
        MatcherAssert.assertThat(
            "the world must hold the object and the marks of the entries, but it doesnt",
            new String(Files.readAllBytes(home.resolve("world.phi")), StandardCharsets.UTF_8),
            Matchers.stringContainsInOrder("gap", "l🌵", "L_entry")
        );
    }

    private static boolean pinned(final Phino phino) {
        boolean found;
        try {
            found = phino.version().equals(phino.pin());
        } catch (final IOException ex) {
            found = false;
        }
        return found;
    }

    private static Path planted(final Path temp) throws IOException {
        final Path home = Files.createDirectories(temp.resolve("lower"));
        Files.write(home.resolve("entries.xmir"), "<object/>".getBytes(StandardCharsets.UTF_8));
        return home;
    }

    private static Path xmir(final Path temp, final String name) throws IOException {
        return Files.write(
            temp.resolve(String.format("%s.xmir", name)),
            "<object/>".getBytes(StandardCharsets.UTF_8)
        );
    }

    private static Phino phino(final Path temp, final String body) throws IOException {
        final Path binary = Files.write(
            temp.resolve("phino"), new ListOf<>("#!/bin/sh", body)
        );
        Files.setPosixFilePermissions(binary, PosixFilePermissions.fromString("rwxr-xr-x"));
        return new Phino(binary.toString());
    }
}
