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
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.io.ResourceOf;
import org.cactoos.list.ListOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.Trimmed;
import org.cactoos.text.UncheckedText;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Lowering}.
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class LoweringTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void createsTheHomeDirectoryWhenTheBinaryReportsThePinnedVersion(@Mktmp final Path temp)
        throws IOException {
        final Path home = temp.resolve("target/eo/7-lower");
        new Lowering(
            new ListOf<>(), LoweringTest.tables(temp), home, LoweringTest.binary(temp)
        ).exec();
        MatcherAssert.assertThat(
            "the lowering must make the directory it was given, but it didnt",
            home.toFile(),
            FileMatchers.anExistingDirectory()
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void runsAnEmptyWorldWhenThereIsNothingToLower(@Mktmp final Path temp)
        throws IOException {
        final Path home = temp.resolve("target/eo/7-lower");
        new Lowering(
            new ListOf<>(), LoweringTest.tables(temp), home, LoweringTest.binary(temp)
        ).exec();
        try (Stream<Path> made = Files.list(home)) {
            MatcherAssert.assertThat(
                "a build with nothing in it must be planted as an empty world, but it wasnt",
                made.map(Path::getFileName).map(Path::toString).collect(Collectors.toList()),
                Matchers.containsInAnyOrder(
                    "entries.xmir", "voids.tsv", "entries.tsv",
                    "world.phi", "atoms.yaml", "protocol.xml"
                )
            );
        }
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void failsWhenTheBinaryReportsAnotherVersion(@Mktmp final Path temp) throws IOException {
        final Path binary = temp.resolve("phino");
        Files.write(binary, new ListOf<>("#!/bin/sh", "echo 0.0.1"));
        Files.setPosixFilePermissions(binary, PosixFilePermissions.fromString("rwxr-xr-x"));
        MatcherAssert.assertThat(
            "the failure must name both the version found and the one pinned, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Lowering(
                    new ListOf<>(),
                    temp.resolve("tables"),
                    temp.resolve("target/eo/7-lower"),
                    binary.toString()
                ).exec(),
                "a binary of another version must fail the lowering"
            ).getMessage(),
            Matchers.stringContainsInOrder("0.0.1", LoweringTest.pin())
        );
    }

    @Test
    void failsNamingTheBinaryThatCannotRun(@Mktmp final Path temp) {
        final Path binary = temp.resolve("absent");
        MatcherAssert.assertThat(
            "the failure must name the binary that is absent, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Lowering(
                    new ListOf<>(),
                    temp.resolve("tables"),
                    temp.resolve("target/eo/7-lower"),
                    binary.toString()
                ).exec(),
                "a binary that is not there must fail the lowering"
            ).getMessage(),
            Matchers.containsString(binary.toString())
        );
    }

    private static String pin() {
        return new UncheckedText(
            new Trimmed(new TextOf(new ResourceOf("org/eolang/lowering/phino-version.txt")))
        ).asString();
    }

    private static String binary(final Path temp) throws IOException {
        final Path made = temp.resolve("phino");
        Files.write(
            made,
            new ListOf<>(
                "#!/bin/sh",
                "case $1 in",
                String.format("--version) echo %s;;", LoweringTest.pin()),
                "merge) while [ $# -gt 0 ]; do [ \"$1\" = --target ] && : > \"$2\"; shift; done;;",
                "morph) for a; do case $a in --protocol=*) : > \"${a#--protocol=}\";; esac; done;;",
                "esac"
            )
        );
        Files.setPosixFilePermissions(made, PosixFilePermissions.fromString("rwxr-xr-x"));
        return made.toString();
    }

    private static Path tables(final Path temp) throws IOException {
        final Path made = Files.createDirectories(temp.resolve("tables"));
        Files.write(
            made.resolve("provides.xml"), "<provides/>".getBytes(StandardCharsets.UTF_8)
        );
        return made;
    }
}
