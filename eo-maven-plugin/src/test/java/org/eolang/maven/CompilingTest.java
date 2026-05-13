/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.Collections;
import org.cactoos.set.SetOf;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Compiling}.
 * @since 0.61.0
 */
final class CompilingTest {

    @Test
    void runsWithoutExceptions(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new Compiling(
                new Assembling(
                    new TjsForeign(),
                    new Parsing(
                        new TjsForeign(),
                        temp,
                        temp,
                        false,
                        "0.0.0",
                        temp
                    ),
                    new Probing(new TjsForeign(), new Objectionary.Fake(), false),
                    new Pulling(
                        new TjsForeign(),
                        temp.resolve(Pulling.DIR),
                        CommitHash.FAKE,
                        new Objectionary.Fake(),
                        temp.resolve(Pulling.CACHE),
                        "0.0.0",
                        false,
                        false,
                        true
                    )
                ),
                new Linting(
                    new TjsForeign(),
                    new TjsForeign(),
                    temp,
                    temp,
                    false,
                    "0.0.0",
                    Collections.emptyList(),
                    Collections.emptyList(),
                    false,
                    false,
                    false,
                    temp,
                    true
                ),
                new Resolving(
                    new TjsForeign(),
                    temp.resolve("resolve"),
                    (dep, path) -> { },
                    false,
                    false,
                    false,
                    true,
                    () -> {
                        throw new IllegalStateException("no runtime expected");
                    },
                    false
                ),
                new Placing(
                    new TjsPlaced(temp.resolve("placed.json")),
                    temp.resolve("nonexistent"),
                    temp,
                    new SetOf<>("**"),
                    new SetOf<>(),
                    false
                )
            ).exec(),
            "Compiling must complete without exceptions for empty input"
        );
    }
}
