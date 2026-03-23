/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link ConcurrentCache}.
 * @since 0.60
 */
@ExtendWith(MktmpResolver.class)
final class ConcurrentCacheTest {

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void triesToCompileProgramConcurrently(@Mktmp final Path temp) throws IOException {
        final AtomicInteger counter = new AtomicInteger(0);
        final ConcurrentCache cache = new ConcurrentCache(
            new Cache(
                temp.resolve("cache"),
                p -> String.format("only once %d", counter.incrementAndGet())
            )
        );
        final Path source = temp.resolve("program.eo");
        new Saved("[] > main\n  (stdout \"Hello, EO!\") > @\n", source).value();
        final Path target = temp.resolve("program.xmir");
        new Threaded<>(
            IntStream.range(0, 100).boxed().collect(Collectors.toList()), ignored -> {
            cache.apply(source, target, source.getFileName());
            return ignored;
        }
        ).total();
        MatcherAssert.assertThat(
            "Program must be compiled only once",
            counter.get(),
            Matchers.equalTo(1)
        );
    }
}
