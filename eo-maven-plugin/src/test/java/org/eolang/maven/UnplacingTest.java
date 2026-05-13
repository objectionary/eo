/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import org.cactoos.set.SetOf;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Unplacing}.
 * @since 0.61.0
 */
final class UnplacingTest {

    @Test
    void skipsWhenNothingIsPlaced(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new Unplacing(
                new TjsPlaced(temp.resolve("placed.json")),
                temp,
                new SetOf<>()
            ).exec(),
            "Unplacing must skip gracefully when the placed catalog is empty"
        );
    }
}
