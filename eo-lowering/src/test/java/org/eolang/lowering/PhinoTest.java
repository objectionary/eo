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
import org.cactoos.list.ListOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Phino}.
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
}
