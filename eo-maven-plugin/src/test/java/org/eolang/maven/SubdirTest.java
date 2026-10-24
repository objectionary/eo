/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;
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
    void skipsANumberHeldByARegularFile(@TempDir final Path temp) throws IOException {
        Files.write(temp.resolve("01-lint"), new byte[0]);
        MatcherAssert.assertThat(
            "a number held by a regular file must be stepped over, not retried until the stack ends (see #9010)",
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
    void waitsWhileAnotherProcessNumbersTheSameTarget(@TempDir final Path temp)
        throws Exception {
        final Path holder = temp.resolve("Holder.java");
        Files.write(
            holder,
            String.join(
                System.lineSeparator(),
                "import java.nio.channels.FileChannel;",
                "import java.nio.file.Paths;",
                "import java.nio.file.StandardOpenOption;",
                "public class Holder {",
                "  public static void main(String[] args) throws Exception {",
                "    try (FileChannel chan = FileChannel.open(Paths.get(args[0]),",
                "      StandardOpenOption.CREATE, StandardOpenOption.WRITE)) {",
                "      chan.lock();",
                "      System.out.println(\"locked\");",
                "      System.out.flush();",
                "      Thread.sleep(2000L);",
                "    }",
                "  }",
                "}"
            ).getBytes(StandardCharsets.UTF_8)
        );
        final Path target = Files.createDirectories(temp.resolve("eo"));
        final Process proc = new ProcessBuilder(
            ProcessHandle.current().info().command().orElse("java"),
            holder.toString(),
            target.resolve(".seqdir.lock").toString()
        ).redirectErrorStream(true).start();
        try (
            BufferedReader out = new BufferedReader(
                new InputStreamReader(proc.getInputStream(), StandardCharsets.UTF_8)
            )
        ) {
            if (!"locked".equals(out.readLine())) {
                throw new IllegalStateException("The other process didn't take the lock");
            }
            final long start = System.nanoTime();
            new Subdir(target, "parse").path();
            MatcherAssert.assertThat(
                "a number must not be reserved while another process holds the lock (see #9013)",
                TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start),
                Matchers.greaterThan(1000L)
            );
        } finally {
            proc.waitFor();
        }
    }
}
