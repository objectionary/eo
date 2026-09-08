/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Unplacing}.
 *
 * @since 0.61.0
 */
final class UnplacingTest {

    @Test
    void skipsWhenNothingIsPlaced(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new Unplacing(
                new TjsPlaced(temp.resolve("placed.json")),
                temp,
                new SetOf<>()
            ).exec(),
            "Unplacing must skip gracefully when the placed catalog is empty"
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void keepsCatalogEntryWhenDeletionFails(@TempDir final Path temp) throws IOException {
        Assumptions.assumeTrue(
            this.protects(temp.resolve("probe")),
            "read-only directories don't stop deletions for this user (root?), can't test"
        );
        final Path classes = temp.resolve("classes");
        Files.createDirectories(classes);
        final Path binary = classes.resolve("Foo.class");
        Files.write(binary, "class-bytes".getBytes(StandardCharsets.UTF_8));
        final TjsPlaced placed = new TjsPlaced(temp.resolve("placed.json"));
        placed.placeClass(binary, "Foo.class", "dep");
        Files.setPosixFilePermissions(classes, PosixFilePermissions.fromString("r-xr-xr-x"));
        try {
            Assertions.assertThrows(
                Exception.class,
                () -> new Unplacing(placed, classes, new SetOf<>()).exec(),
                "a deletion failure must surface as an exception, not be swallowed"
            );
        } finally {
            Files.setPosixFilePermissions(classes, PosixFilePermissions.fromString("rwxr-xr-x"));
        }
        MatcherAssert.assertThat(
            "the catalog entry must remain placed after a failed deletion",
            placed.classes().iterator().next().placed(),
            Matchers.is(true)
        );
    }

    private boolean protects(final Path dir) throws IOException {
        Files.createDirectories(dir);
        final Path file = dir.resolve("probe.txt");
        Files.write(file, "probe".getBytes(StandardCharsets.UTF_8));
        Files.setPosixFilePermissions(dir, PosixFilePermissions.fromString("r-xr-xr-x"));
        boolean protects;
        try {
            Files.deleteIfExists(file);
            protects = false;
        } catch (final IOException ex) {
            protects = true;
        } finally {
            Files.setPosixFilePermissions(dir, PosixFilePermissions.fromString("rwxr-xr-x"));
        }
        return protects;
    }
}
