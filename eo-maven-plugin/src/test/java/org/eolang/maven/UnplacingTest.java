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
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.apache.log4j.Appender;
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test cases for {@link Unplacing}.
 *
 * @since 0.61.0
 */
@Execution(ExecutionMode.SAME_THREAD)
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

    @Test
    void countsOnlyUnkeptBinariesWhenNoneIsDeleted(@TempDir final Path temp) throws IOException {
        final long seed = System.nanoTime();
        final Path classes = temp.resolve("classes");
        final Path binary = classes.resolve("kept/Foo.class");
        this.write(binary);
        for (int idx = new Random(seed).nextInt(5); idx >= 0; --idx) {
            this.write(classes.resolve(String.format("kept/Бар%d.class", idx)));
        }
        final TjsPlaced placed = new TjsPlaced(temp.resolve("placed.json"));
        placed.placeClass(binary, "kept/Foo.class", "dep");
        MatcherAssert.assertThat(
            String.format("kept binaries are counted as eligible for deletion, seed %d", seed),
            this.messages(new Unplacing(placed, classes, new SetOf<>("kept/**"))),
            Matchers.hasItem(Matchers.startsWith("No binaries out of 0 deleted"))
        );
    }

    @Test
    void countsOnlyUnkeptBinariesWhenSomeAreDeleted(@TempDir final Path temp) throws IOException {
        final long seed = System.nanoTime();
        final Path classes = temp.resolve("classes");
        final Path binary = classes.resolve("Foo.class");
        this.write(binary);
        this.write(classes.resolve("Бар.class"));
        for (int idx = new Random(seed).nextInt(5); idx >= 0; --idx) {
            this.write(classes.resolve(String.format("kept/Баз%d.class", idx)));
        }
        final TjsPlaced placed = new TjsPlaced(temp.resolve("placed.json"));
        placed.placeClass(binary, "Foo.class", "dep");
        MatcherAssert.assertThat(
            String.format("kept binaries are counted as eligible for deletion, seed %d", seed),
            this.messages(new Unplacing(placed, classes, new SetOf<>("kept/**"))),
            Matchers.hasItem(Matchers.startsWith("Just 1 binari(es) out of 2 deleted"))
        );
    }

    private void write(final Path file) throws IOException {
        Files.createDirectories(file.getParent());
        Files.write(file, "class-bytes".getBytes(StandardCharsets.UTF_8));
    }

    private List<String> messages(final Unplacing unplacing) throws IOException {
        final List<String> messages = new ArrayList<>(0);
        final Appender appender = new AppenderSkeleton() {
            @Override
            protected void append(final LoggingEvent event) {
                messages.add(String.valueOf(event.getRenderedMessage()));
            }

            @Override
            public void close() {
                // Nothing to release.
            }

            @Override
            public boolean requiresLayout() {
                return false;
            }
        };
        final Logger logger = Logger.getLogger(Unplacing.class);
        final Level level = logger.getLevel();
        logger.setLevel(Level.INFO);
        logger.addAppender(appender);
        try {
            unplacing.exec();
        } finally {
            logger.removeAppender(appender);
            logger.setLevel(level);
        }
        return messages;
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
