/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
}
