/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@code file.touched}.
 * @since 0.75.0
 */
final class EOfileEOtouchedTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void createsTheTargetOfADanglingSymlink(@TempDir final Path dir) throws IOException {
        final Path target = dir.resolve("target");
        final Path link = dir.resolve("link");
        Files.createSymbolicLink(link, target);
        new Dataized(this.touched(link)).take();
        MatcherAssert.assertThat(
            "touching a dangling symlink should create its target, but it didn't",
            Files.exists(target),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "touching a dangling symlink should keep the link itself a symlink, but it didn't",
            Files.isSymbolicLink(link),
            Matchers.is(true)
        );
    }

    private Phi touched(final Path path) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(path.toString()));
        return file.take("touched");
    }
}
