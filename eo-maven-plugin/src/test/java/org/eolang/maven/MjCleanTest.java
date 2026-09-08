/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Set;
import java.util.function.BiConsumer;
import org.apache.maven.model.Dependency;
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjClean}.
 * @since 0.28.6
 */
@ExtendWith(MktmpResolver.class)
final class MjCleanTest {

    @Test
    void cleansSuccessfully(@Mktmp final Path temp) throws IOException {
        final Path dir = Files.createDirectories(temp.resolve("target"));
        final Path small = Files.createDirectories(
            Files.createDirectories(dir.resolve("child")).resolve("child.eo")
        );
        final Path file = Files.createTempFile(dir, "some", ".eo");
        if (!small.toFile().exists() || !file.toFile().exists()) {
            throw new IllegalStateException("Files not created.");
        }
        new FakeMaven(temp)
            .with("targetDir", dir.toFile())
            .execute(MjClean.class);
        MatcherAssert.assertThat(
            "CleanMojo should delete all temp files and directories, but it doesn't",
            !file.toFile().exists() && !small.toFile().exists(),
            Matchers.is(true)
        );
    }

    @Test
    void complainsWhenTheDirectoryStays(@Mktmp final Path temp) throws IOException {
        final Path dir = Files.createDirectories(temp.resolve("target"));
        final Path kept = Files.createDirectories(dir.resolve("kept"));
        Files.writeString(kept.resolve("stale.eo"), "# nothing");
        try {
            Files.setPosixFilePermissions(kept, Set.of(PosixFilePermission.OWNER_READ));
        } catch (final UnsupportedOperationException ex) {
            Assumptions.abort("this file system has no POSIX permissions");
        }
        Assumptions.assumeFalse(
            Files.isWritable(kept), "this user deletes whatever the permissions say"
        );
        try {
            Assertions.assertThrows(
                Exception.class,
                () -> new FakeMaven(temp).with("targetDir", dir.toFile()).execute(MjClean.class),
                "a directory that could not be deleted must not pass for a clean one"
            );
        } finally {
            Files.setPosixFilePermissions(
                kept,
                Set.of(
                    PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE,
                    PosixFilePermission.OWNER_EXECUTE
                )
            );
        }
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void makesFullCompilingLifecycleSuccessfully(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp)
            .withHelloWorld()
            .with("included", new SetOf<>("**.eo"))
            .with("classesDir", temp.resolve("out").toFile())
            .with("placed", temp.resolve("list").toFile())
            .with("cache", temp.resolve("cache/parsed").toFile())
            .with("skipZeroVersions", true).with(
                "central",
                (BiConsumer<Dependency, Path>) (dependency, path) -> {
                    assert dependency != null;
                }
            )
            .execute(MjRegister.class)
            .execute(MjAssemble.class)
            .execute(MjClean.class);
        MatcherAssert.assertThat(
            "CleanMojo should delete all files after full compiling lifecycle, but it doesn't",
            temp.resolve("target").toFile().exists(),
            Matchers.is(false)
        );
    }
}
