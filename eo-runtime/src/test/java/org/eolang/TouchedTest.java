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
 * Test case for {@code file.touched} on a dangling symbolic link.
 * @since 0.75.0
 */
final class TouchedTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void createsTheTargetOfADanglingSymlink(@TempDir final Path temp) throws IOException {
        final Path target = temp.resolve("target");
        final Path link = temp.resolve("link");
        Files.createSymbolicLink(link, target);
        new Dataized(
            new PhApplication(
                Phi.Φ.take("file").copy(), 0, new Data.ToPhi(link.toString())
            ).take("touched")
        ).take();
        MatcherAssert.assertThat(
            "file.touched on a dangling symlink must create the link's target",
            Files.exists(target),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "file.touched must not replace the symlink itself with a regular file",
            Files.isSymbolicLink(link),
            Matchers.is(true)
        );
    }
}
