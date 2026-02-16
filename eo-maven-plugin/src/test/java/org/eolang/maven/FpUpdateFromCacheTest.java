/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.Text;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link FpUpdateFromCache}.
 * @since 0.57
 */
@ExtendWith(MktmpResolver.class)
final class FpUpdateFromCacheTest {

    @Test
    void appliesFromCache(@Mktmp final Path tmp) throws IOException {
        final Text expected = new TextOf("Cached!");
        final Path cache = tmp.resolve("cache.txt");
        new Saved(expected, cache).value();
        final Path target = tmp.resolve("target.txt");
        MatcherAssert.assertThat(
            "We expect the footprint will return the target file path",
            new FpUpdateFromCache(() -> cache).apply(Paths.get("/dev/null"), target),
            Matchers.equalTo(target)
        );
        MatcherAssert.assertThat(
            "The target file should be updated from cache",
            new TextOf(target),
            Matchers.equalTo(expected)
        );
    }
}
