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
import java.time.Duration;
import org.cactoos.list.ListOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests of the class {@link Phino}.
 *
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
final class PhinoTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void readsTheVersionABinaryPrints(@Mktmp final Path temp) throws IOException {
        final Path binary = temp.resolve("phino");
        Files.write(
            binary, new ListOf<>("#!/bin/sh", "echo 0.4.2")
        );
        Files.setPosixFilePermissions(
            binary, PosixFilePermissions.fromString("rwxr-xr-x")
        );
        MatcherAssert.assertThat(
            "the version must be what the binary printed, but it isnt",
            new Phino(binary.toString()).version(),
            Matchers.equalTo("0.4.2")
        );
    }

    @Test
    void namesTheBinaryThatCannotRun(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the failure must name the binary that is absent, but it doesnt",
            Assertions.assertThrows(
                IOException.class,
                () -> new Phino(temp.resolve("absent").toString()).version(),
                "a binary that is not there cannot report a version"
            ).getMessage(),
            Matchers.containsString("absent")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void handsPhinoTheBudgetInSeconds(@Mktmp final Path temp) throws IOException {
        new Phino(PhinoTest.binary(temp, "echo \"$@\" > \"${0%/*}/args.txt\"")).morph(
            temp.resolve("world.phi"), temp.resolve("atoms.yaml"), 4,
            temp.resolve("4.xml"), 32, Duration.ofSeconds(7L)
        );
        MatcherAssert.assertThat(
            "phino must be told the budget in seconds, but it isnt",
            new String(Files.readAllBytes(temp.resolve("args.txt")), StandardCharsets.UTF_8),
            Matchers.containsString("--max-seconds=7")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void roundsABudgetShorterThanASecondUpToOne(@Mktmp final Path temp) throws IOException {
        new Phino(PhinoTest.binary(temp, "echo \"$@\" > \"${0%/*}/args.txt\"")).morph(
            temp.resolve("world.phi"), temp.resolve("atoms.yaml"), 9,
            temp.resolve("9.xml"), 16, Duration.ofMillis(300L)
        );
        MatcherAssert.assertThat(
            "a budget under a second must reach phino as one second, but it doesnt",
            new String(Files.readAllBytes(temp.resolve("args.txt")), StandardCharsets.UTF_8),
            Matchers.containsString("--max-seconds=1")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void letsPhinoWorkPastItsBudget(@Mktmp final Path temp) throws IOException {
        final Phino phino = new Phino(PhinoTest.binary(temp, "sleep 2"));
        Assertions.assertDoesNotThrow(
            () -> phino.morph(
                temp.resolve("world.phi"), temp.resolve("atoms.yaml"), 5,
                temp.resolve("5.xml"), 32, Duration.ofMillis(500L)
            ),
            "phino must stop by itself, and never be killed from Java"
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void reportsAMorphingThatRanOutOfItsBudget(@Mktmp final Path temp) throws IOException {
        final Phino phino = new Phino(
            PhinoTest.binary(
                temp,
                "for a; do case $a in --protocol=*) p=${a#--protocol=};; esac; done;",
                "echo '<morph><timeout limit=\"2\"/></morph>' > \"$p\"; exit 1"
            )
        );
        MatcherAssert.assertThat(
            "the failure must name the budget the run ran out of, but it doesnt",
            Assertions.assertThrows(
                KilledException.class,
                () -> phino.morph(
                    temp.resolve("world.phi"), temp.resolve("atoms.yaml"), 3,
                    temp.resolve("3.xml"), 32, Duration.ofSeconds(2L)
                ),
                "a run that phino stopped on time must be reported as one"
            ).getMessage(),
            Matchers.containsString("2s")
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void failsAMorphingThatBreaksWithinItsBudget(@Mktmp final Path temp) throws IOException {
        final Phino phino = new Phino(PhinoTest.binary(temp, "echo broken >&2; exit 3"));
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> phino.morph(
                temp.resolve("world.phi"), temp.resolve("atoms.yaml"), 6,
                temp.resolve("6.xml"), 32, Duration.ofSeconds(4L)
            ),
            "a run that broke without a timeout must not be taken for one that ran out of time"
        );
    }

    private static String binary(final Path temp, final String... lines) throws IOException {
        final Path made = Files.write(
            temp.resolve("phino"),
            String.format("#!/bin/sh%n%s%n", String.join(" ", lines))
                .getBytes(StandardCharsets.UTF_8)
        );
        Files.setPosixFilePermissions(made, PosixFilePermissions.fromString("rwxr-xr-x"));
        return made.toString();
    }
}
