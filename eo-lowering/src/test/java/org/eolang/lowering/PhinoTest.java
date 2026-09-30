/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
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
 * @since 0.74.0
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
    void stopsAMorphingThatOutlastsItsBudget(@Mktmp final Path temp) throws IOException {
        final Path binary = Files.write(
            temp.resolve("phino"), new ListOf<>("#!/bin/sh", "exec sleep 30")
        );
        Files.setPosixFilePermissions(
            binary, PosixFilePermissions.fromString("rwxr-xr-x")
        );
        MatcherAssert.assertThat(
            "the failure must name the budget the run outlasted, but it doesnt",
            Assertions.assertThrows(
                KilledException.class,
                () -> new Phino(binary.toString()).morph(
                    temp.resolve("world.phi"), temp.resolve("atoms.yaml"), 3,
                    temp.resolve("3.xml"), 32, Duration.ofMillis(700L)
                ),
                "a run over its budget must be stopped"
            ).getMessage(),
            Matchers.containsString("700ms")
        );
    }
}
