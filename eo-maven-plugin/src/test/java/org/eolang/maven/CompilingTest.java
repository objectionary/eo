/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Compiling}.
 * @since 0.61.0
 */
final class CompilingTest {

    @Test
    void runsEveryStepInTheOrderItIsGiven() throws IOException {
        final Collection<String> done = new ArrayList<>(5);
        new Compiling(
            () -> done.add("assembling"),
            () -> done.add("linting"),
            () -> done.add("merging"),
            () -> done.add("resolving"),
            () -> done.add("placing")
        ).exec();
        MatcherAssert.assertThat(
            "every step must run once, in the order it was handed over, but they didnt",
            done,
            Matchers.contains(
                "assembling", "linting", "merging", "resolving", "placing"
            )
        );
    }

    @Test
    void runsWithoutExceptions(@TempDir final Path temp) {
        Assertions.assertDoesNotThrow(
            new Compiling(
                new Assembling(
                    new TjsForeign(),
                    new Parsing(
                        new TjsForeign(),
                        temp,
                        temp,
                        new GlobalCache.GcFresh()
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
                    true
                ),
                new Merging(new TjsForeign(), temp.resolve(Merging.DIR)),
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
            )::exec,
            "Compiling must complete without exceptions for empty input"
        );
    }
}
