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
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjClean}.
 *
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
    @ExtendWith(WeAreOnline.class)
    void makesFullCompilingLifecycleSuccessfully(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp)
            .withHelloWorld()
            .with("includeSources", new SetOf<>("**.eo"))
            .with("classesDir", temp.resolve("out").toFile())
            .with("placed", temp.resolve("list").toFile())
            .with("cache", temp.resolve("cache/parsed").toFile())
            .with("skipZeroVersions", true)
            .with("central", Central.EMPTY)
            .with("ignoreTransitive", true)
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
