/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link FpIfTargetOlder}.
 * @since 0.58
 */
final class FpIfTargetOlderTest {

    @Test
    void choosesTheFirstFootprintWhenTargetIsOlder(@TempDir final Path tmp) throws IOException {
        final Path source = tmp.resolve("source");
        final Path target = tmp.resolve("target");
        FpIfTargetOlderTest.touch(source, Instant.now());
        FpIfTargetOlderTest.touch(target, Instant.now().minusSeconds(60));
        Assertions.assertEquals(
            source,
            new FpIfTargetOlder(
                t -> t,
                (src, tgt) -> source,
                (src, tgt) -> target
            ).apply(source, target),
            "Should choose the first footprint when the target is older than the source"
        );
    }

    @Test
    void choosesTheSecondFootprintWhenTargetIsNewer(@TempDir final Path tmp) throws IOException {
        final Path source = tmp.resolve("source");
        final Path target = tmp.resolve("target");
        FpIfTargetOlderTest.touch(source, Instant.now().minusSeconds(60));
        FpIfTargetOlderTest.touch(target, Instant.now());
        Assertions.assertEquals(
            target,
            new FpIfTargetOlder(
                t -> t,
                (src, tgt) -> source,
                (src, tgt) -> target
            ).apply(source, target),
            "Should choose the second footprint when the target is newer than the source"
        );
    }

    /**
     * Create a file with the given last modified time.
     * @param path Path to the file
     * @param mtime Last modified time to set
     * @throws IOException If fails to create or touch the file
     */
    private static void touch(final Path path, final Instant mtime) throws IOException {
        new Saved("", path).value();
        Files.setLastModifiedTime(path, FileTime.from(mtime));
    }
}
