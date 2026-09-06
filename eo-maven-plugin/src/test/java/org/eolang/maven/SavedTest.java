/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.cactoos.scalar.Unchecked;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Saved}.
 * @since 0.74
 */
@ExtendWith(MktmpResolver.class)
final class SavedTest {

    @Test
    void savesContentToFile(@Mktmp final Path temp) throws IOException {
        final Path target = temp.resolve("out.txt");
        new Saved("hello", target).value();
        MatcherAssert.assertThat(
            "the saved file must contain exactly what was written",
            Files.readString(target, StandardCharsets.UTF_8),
            Matchers.equalTo("hello")
        );
    }

    @Test
    void savesContentToFileWithShortName(@Mktmp final Path temp) throws IOException {
        final Path target = temp.resolve("x");
        new Saved("hi", target).value();
        MatcherAssert.assertThat(
            "a one-character file name must be saved, not refused by the temporary prefix",
            Files.readString(target, StandardCharsets.UTF_8),
            Matchers.equalTo("hi")
        );
    }

    @Test
    void savesContentToRelativeFilenameWithoutParentDirectory() throws IOException {
        final Path target = Path.of(String.format("saved-%s.tmp", UUID.randomUUID()));
        try {
            new Saved("hello", target).value();
            MatcherAssert.assertThat(
                "a relative filename with no parent directory must be saved as-is",
                Files.readString(target, StandardCharsets.UTF_8),
                Matchers.equalTo("hello")
            );
        } finally {
            Files.deleteIfExists(target);
        }
    }

    @Test
    void exposesNoPartiallyWrittenFileToConcurrentReader(
        @Mktmp final Path temp
    ) throws Exception {
        final Path target = temp.resolve("shared.txt");
        final List<String> variants = IntStream.range(0, 8)
            .mapToObj(idx -> String.valueOf((char) ('a' + idx)).repeat(500_000))
            .collect(Collectors.toList());
        new Saved(variants.get(0), target).value();
        final List<String> broken = new CopyOnWriteArrayList<>();
        new Threaded<>(
            variants,
            variant -> {
                new Saved(variant, target).value();
                final String read = Files.readString(target, StandardCharsets.UTF_8);
                if (!variants.contains(read)) {
                    broken.add(read.substring(0, Math.min(read.length(), 40)));
                }
                return 1;
            }
        ).total();
        MatcherAssert.assertThat(
            "a reader racing with concurrent writers must always see a complete variant, never a mix of two",
            broken,
            Matchers.empty()
        );
    }

    @Test
    void createsMissingParentDirectories(@Mktmp final Path temp) throws Exception {
        final Path target = temp.resolve("deep/nested/tree/out.txt");
        new Saved("qwerty-42", target).value();
        MatcherAssert.assertThat(
            "the file must be saved even when its parent directories dont exist yet",
            Files.readString(target, StandardCharsets.UTF_8),
            Matchers.equalTo("qwerty-42")
        );
    }

    @Test
    void overwritesContentOfExistingFile(@Mktmp final Path temp) throws Exception {
        final Path target = temp.resolve("twice.txt");
        new Saved("first-7", target).value();
        new Saved("second-13", target).value();
        MatcherAssert.assertThat(
            "the second write must replace the content left by the first one",
            Files.readString(target, StandardCharsets.UTF_8),
            Matchers.equalTo("second-13")
        );
    }

    @Test
    void leavesNoTemporaryFilesBehind(@Mktmp final Path temp) throws Exception {
        new Saved("zzz-99", temp.resolve("clean.txt")).value();
        try (Stream<Path> files = Files.list(temp)) {
            MatcherAssert.assertThat(
                "the temporary file must not survive a successful save",
                files.map(path -> path.getFileName().toString()).collect(Collectors.toList()),
                Matchers.contains("clean.txt")
            );
        }
    }

    @Test
    void retriesMoveWhenTargetTemporarilyBlocked(@Mktmp final Path temp) throws Exception {
        final Path target = temp.resolve("blocked.txt");
        Files.createDirectories(target);
        final Thread writer = new Thread(
            () -> new Unchecked<>(new Saved("content", target)).value()
        );
        writer.start();
        Thread.sleep(300L);
        Files.delete(target);
        writer.join();
        MatcherAssert.assertThat(
            "the write must succeed once a retry lands after the obstruction clears",
            Files.readString(target, StandardCharsets.UTF_8),
            Matchers.equalTo("content")
        );
    }
}
