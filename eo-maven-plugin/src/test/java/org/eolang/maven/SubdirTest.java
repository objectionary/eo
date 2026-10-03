/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.eolang.cache.Saved;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Subdir}.
 *
 * @since 0.72.0
 */
final class SubdirTest {

    @Test
    void numbersTheFirstNameAskedForAsOne(@TempDir final Path temp) {
        MatcherAssert.assertThat(
            "the first name asked for under an empty target must be numbered 01",
            new Subdir(temp, "parse").path(),
            Matchers.equalTo(temp.resolve("01-parse"))
        );
    }

    @Test
    void keepsTheSameNumberWhenAskedTwice(@TempDir final Path temp) {
        MatcherAssert.assertThat(
            "the same name must keep the number it was given the first time",
            new Subdir(temp, "resolve").path(),
            Matchers.equalTo(new Subdir(temp, "resolve").path())
        );
    }

    @Test
    void numbersDistinctNamesDifferently(@TempDir final Path temp) {
        MatcherAssert.assertThat(
            "two different names must never land on the same number",
            new Subdir(temp, "parse").path(),
            Matchers.not(Matchers.equalTo(new Subdir(temp, "lint").path()))
        );
    }

    @Test
    void reusesTheNumberAlreadyOnDisk(@TempDir final Path temp) throws IOException {
        Files.createDirectories(temp.resolve("03-lint"));
        MatcherAssert.assertThat(
            "a name with a directory already on disk must keep that number, not restart from 01",
            new Subdir(temp, "lint").path(),
            Matchers.equalTo(temp.resolve("03-lint"))
        );
    }

    @Test
    void skipsNumbersAlreadyTakenOnDisk(@TempDir final Path temp) throws IOException {
        Files.createDirectories(temp.resolve("01-parse"));
        Files.createDirectories(temp.resolve("04-merge"));
        MatcherAssert.assertThat(
            "a brand new name must be given the number past the highest one already on disk",
            new Subdir(temp, "resolve").path(),
            Matchers.equalTo(temp.resolve("05-resolve"))
        );
    }

    @Test
    void doesNotShiftAStableNameWhenAnEarlierOneIsMissingFromThisRun(
        @TempDir final Path temp
    ) throws IOException {
        Files.createDirectories(temp.resolve("01-parse"));
        Files.createDirectories(temp.resolve("02-lint"));
        MatcherAssert.assertThat(
            "a stage that this run never asked for must not push a later stage onto a new number",
            new Subdir(temp, "lint").path(),
            Matchers.equalTo(temp.resolve("02-lint"))
        );
    }

    @Test
    void createsTheDirectoryItNumbers(@TempDir final Path temp) {
        MatcherAssert.assertThat(
            "the numbered directory must exist as soon as it is numbered",
            Files.isDirectory(new Subdir(temp, "transpile").path()),
            Matchers.is(true)
        );
    }

    @Test
    void reportsAFileBlockingTheNextDirectory(
        @TempDir final Path temp
    ) throws IOException {
        final String content = "not a stage directory";
        final Path blocked = new Saved(
            content, temp.resolve("01-lint")
        ).value();
        Throwable failure;
        try {
            new Subdir(temp, "lint").path();
            failure = new IllegalStateException("no collision was reported");
        } catch (final UncheckedIOException | StackOverflowError err) {
            failure = err;
        }
        final Throwable cause = failure.getCause();
        final String causeclass;
        final String message;
        if (cause == null) {
            causeclass = "";
            message = "";
        } else {
            causeclass = cause.getClass().getName();
            message = cause.getMessage();
        }
        final List<Path> entries;
        try (Stream<Path> stream = Files.list(temp)) {
            entries = stream.collect(Collectors.toList());
        }
        MatcherAssert.assertThat(
            "the file collision must be reported without changing the target",
            String.format(
                "%s|%s|%s|%s|%s",
                failure.getClass().getName(),
                causeclass,
                message,
                Files.readString(blocked),
                entries
            ),
            Matchers.equalTo(
                String.format(
                    "%s|%s|%s|%s|[%s]",
                    UncheckedIOException.class.getName(),
                    FileAlreadyExistsException.class.getName(),
                    blocked,
                    content,
                    blocked
                )
            )
        );
    }
}
