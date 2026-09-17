/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link Caching}.
 *
 * @since 0.74
 */
@ExtendWith(MktmpResolver.class)
final class CachingTest {

    @Test
    void servesTheSecondRunFromTheCacheWhenEnabled(@Mktmp final Path temp) throws IOException {
        final Path source = temp.resolve("source.eo");
        Files.writeString(source, "[] > main", StandardCharsets.UTF_8);
        final Path target = temp.resolve("target.xmir");
        final AtomicInteger counter = new AtomicInteger(0);
        final GlobalCache cache = new Caching(temp.resolve("cache").toFile(), true, "1.2.3")
            .forStep("step");
        for (int idx = 0; idx < 2; ++idx) {
            cache.footprint(
                Path.of("target.xmir"),
                () -> "0123456789",
                src -> String.format("compiled %d", counter.incrementAndGet())
            ).apply(source, target);
        }
        MatcherAssert.assertThat(
            "the second run must be written from the cache, not compiled again",
            Files.readString(target, StandardCharsets.UTF_8),
            Matchers.equalTo("compiled 1")
        );
    }

    @Test
    void compilesAgainOnEveryRunWhenDisabled(@Mktmp final Path temp) throws IOException {
        final Path source = temp.resolve("source.eo");
        Files.writeString(source, "[] > main", StandardCharsets.UTF_8);
        final Path target = temp.resolve("target.xmir");
        final AtomicInteger counter = new AtomicInteger(0);
        final GlobalCache cache = new Caching(temp.resolve("cache").toFile(), false, "1.2.3")
            .forStep("step");
        for (int idx = 0; idx < 2; ++idx) {
            cache.footprint(
                Path.of("target.xmir"),
                () -> "0123456789",
                src -> String.format("compiled %d", counter.incrementAndGet())
            ).apply(source, target);
        }
        MatcherAssert.assertThat(
            "nothing must be remembered between two runs when caching is disabled",
            Files.readString(target, StandardCharsets.UTF_8),
            Matchers.equalTo("compiled 2")
        );
    }
}
