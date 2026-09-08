/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link EmptyDirectoriesIn}.
 * @since 0.55
 */
@ExtendWith(MktmpResolver.class)
final class EmptyDirectoriesInTest {

    @Test
    void deletesAnEmptyDirectory(@Mktmp final Path temp) throws IOException {
        final Path classes = Files.createDirectories(temp.resolve("classes"));
        final Path empty = Files.createDirectories(classes.resolve("org/eolang"));
        new EmptyDirectoriesIn(classes).clear();
        MatcherAssert.assertThat(
            "an empty directory of the build output must be pruned, but it wasnt",
            Files.exists(empty),
            Matchers.equalTo(false)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void keepsADirectoryBehindALink(@Mktmp final Path temp) throws IOException {
        final Path outside = Files.createDirectories(temp.resolve("outside/keep-me"));
        final Path classes = Files.createDirectories(temp.resolve("classes"));
        Files.createSymbolicLink(classes.resolve("linked"), temp.resolve("outside"));
        new EmptyDirectoriesIn(classes).clear();
        MatcherAssert.assertThat(
            "a directory reached through a link was never created by EO, so it must not be deleted, but it was",
            Files.exists(outside),
            Matchers.equalTo(true)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void keepsTheLinkAndTheDirectoryHoldingIt(@Mktmp final Path temp) throws IOException {
        final Path classes = Files.createDirectories(temp.resolve("classes"));
        final Path holder = Files.createDirectories(classes.resolve("holder"));
        final Path link = holder.resolve("linked");
        Files.createSymbolicLink(link, Files.createDirectories(temp.resolve("outside")));
        new EmptyDirectoriesIn(classes).clear();
        MatcherAssert.assertThat(
            "a link is something a directory holds, so neither it nor its directory is empty, but they were pruned",
            Files.isSymbolicLink(link),
            Matchers.equalTo(true)
        );
    }
}
