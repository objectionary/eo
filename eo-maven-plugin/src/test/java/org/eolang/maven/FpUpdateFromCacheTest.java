/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.Text;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link FpUpdateFromCache}.
 * @since 0.57
 */
final class FpUpdateFromCacheTest {

    @Test
    void appliesFromCache() throws IOException {
        final Filesystem.Fake fake = new Filesystem.Fake();
        final Text expected = new TextOf("Cached!");
        final Path cached = fake.save(Paths.get("./cache.txt"), expected);
        final Path target = Paths.get("./target.txt");
        MatcherAssert.assertThat(
            "We expect the footprint will return the target file path",
            new FpUpdateFromCache(() -> cached, fake).apply(Paths.get("/dev/null"), target),
            Matchers.equalTo(target)
        );
        MatcherAssert.assertThat(
            "The target file should be updated from cache",
            fake.read(target),
            Matchers.equalTo(expected)
        );
    }
}
