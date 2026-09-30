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
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Wiping}.
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class WipingTest {

    @Test
    void deletesTheDirectoryWithAllItHolds(@Mktmp final Path temp) throws IOException {
        final Path dir = temp.resolve("kept");
        Files.write(
            Files.createDirectories(dir.resolve("org/eolang")).resolve("EOx.java"),
            new byte[] {0x2F, 0x2A}
        );
        new Wiping().exec(dir);
        MatcherAssert.assertThat(
            "the wiped directory must be gone with all it held, but it stays",
            Files.exists(dir),
            Matchers.is(false)
        );
    }

    @Test
    void leavesNothingBehindWhereThereWasNothing(@Mktmp final Path temp) throws IOException {
        final Path dir = temp.resolve("absent");
        new Wiping().exec(dir);
        MatcherAssert.assertThat(
            "wiping a directory that is not there must make none, but it did",
            Files.exists(dir),
            Matchers.is(false)
        );
    }
}
