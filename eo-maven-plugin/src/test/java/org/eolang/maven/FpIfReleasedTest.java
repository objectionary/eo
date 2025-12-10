/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link FpIfReleased}.
 * @since 0.57
 */
final class FpIfReleasedTest {

    @Test
    void throwsNpeIfHashIsNull(@TempDir final Path tmp) {
        Assertions.assertThrows(
            NullPointerException.class,
            () -> new FpIfReleased(
                "1.2.3",
                () -> null,
                (source, target) -> source,
                (source, target) -> target
            ).apply(tmp.resolve("src"), tmp.resolve("tgt")),
            "Should throw NPE if hash is null"
        );
    }

    @Test
    void choosesTheFirstFootprintBecauseCacheable(@TempDir final Path tmp) throws IOException {
        final Path src = tmp.resolve("first");
        final Path result = new FpIfReleased(
            "1.2.3",
            () -> "somehash",
            (source, target) -> source,
            (source, target) -> target
        ).apply(src, tmp.resolve("second"));
        Assertions.assertEquals(
            src,
            result,
            "Should choose the first footprint when cacheable"
        );
    }

    @Test
    void choosesTheSecondFootprintBecauseNotCacheable(@TempDir final Path tmp) throws IOException {
        final Path tgt = tmp.resolve("right");
        final Path result = new FpIfReleased(
            "1.2.3-SNAPSHOT",
            () -> "contains-snapshot-word",
            (source, target) -> source,
            (source, target) -> target
        ).apply(tmp.resolve("left"), tgt);
        Assertions.assertEquals(
            tgt,
            result,
            "Should choose the second footprint when not cacheable"
        );
    }

    @Test
    void choosesTheSecondFootprintBecauseVersionIsZero(@TempDir final Path tmp) throws IOException {
        final Path tgt = tmp.resolve("right-zero");
        final Path result = new FpIfReleased(
            "0.0.0",
            () -> "any-hash-value",
            (source, target) -> source,
            (source, target) -> target
        ).apply(tmp.resolve("left-zero"), tgt);
        Assertions.assertEquals(
            tgt,
            result,
            "Should choose the second footprint when version is 0.0.0"
        );
    }

    @Test
    void choosesTheSecondFootprintBecauseHashIsEmpty(@TempDir final Path tmp) throws IOException {
        final Path tgt = tmp.resolve("right-empty-hash");
        final Path result = new FpIfReleased(
            "1.2.3",
            () -> "",
            (source, target) -> source,
            (source, target) -> target
        ).apply(tmp.resolve("left-empty-hash"), tgt);
        Assertions.assertEquals(
            tgt,
            result,
            "Should choose the second footprint when hash is empty"
        );
    }
}
