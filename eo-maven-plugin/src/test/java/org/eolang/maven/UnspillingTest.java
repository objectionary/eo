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
 * Test cases for {@link Unspiling}.
 * @since 0.61.0
 */
final class UnspillingTest {

    @Test
    void skipsWhenNoClassesExist(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new Unspiling(
                temp.resolve("generated"),
                temp.resolve("classes"),
                new SetOf<>()
            ).exec(),
            "Unspiling must skip gracefully when the classes directory is empty"
        );
    }
}
