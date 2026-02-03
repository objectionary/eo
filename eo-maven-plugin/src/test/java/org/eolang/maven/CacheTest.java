/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link Cache}.
 * @since 0.60
 * @todo #4846:30min Add more tests for Cache class.
 *  Currently only two basic tests are implemented. More tests should be added to cover edge cases
 *  and ensure robustness of the caching mechanism.
 *  For example, you can add a test to verify the behavior when the source file is modified.
 */
@ExtendWith(MktmpResolver.class)
final class CacheTest {

    @Test
    void compilesSourceAndAddsToCache(@Mktmp final Path temp) throws Exception {
        final var base = temp.resolve("cache");
        Files.createDirectories(base);
        final var source = temp.resolve("source.eo");
        Files.writeString(source, "[] > main\n  (stdout \"Hello, EO!\") > @\n");
        final var target = temp.resolve("target.xmir");
        final var tail = source.getFileName();
        final String content = "compiled";
        new Cache(base, p -> content).apply(source, target, tail);
        MatcherAssert.assertThat(
            "Target file must be created from source",
            Files.readString(target),
            Matchers.equalTo(content)
        );
        MatcherAssert.assertThat(
            "Cache file must be created",
            Files.exists(base.resolve(tail)),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "Hash file must be created",
            Files.exists(base.resolve(String.format("%s.sha256", tail))),
            Matchers.is(true)
        );
    }

    @Test
    void readsFromCacheWhenUnchanged(@Mktmp final Path temp) throws Exception {
        final var base = temp.resolve("cache-directory");
        Files.createDirectories(base);
        final var source = temp.resolve("stdin.eo");
        Files.writeString(source, "[] > main\n  (stdout \"Hello, EO!\") > @\n");
        final var target = temp.resolve("stdin.xmir");
        final var counter = new AtomicInteger(0);
        final var cache = new Cache(
            base,
            p -> String.format("stdin %d", counter.incrementAndGet())
        );
        final Path tail = source.getFileName();
        cache.apply(source, target, tail);
        MatcherAssert.assertThat(
            "Compilation should happen only once",
            counter.get(),
            Matchers.equalTo(1)
        );
        cache.apply(source, target, tail);
        MatcherAssert.assertThat(
            "Compilation should not happen again",
            counter.get(),
            Matchers.equalTo(1)
        );
    }
}
