/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import org.cactoos.Scalar;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Resolving}.
 * @since 0.61.0
 */
@ExtendWith(MktmpResolver.class)
final class ResolvingTest {

    @Test
    void cleansUpSharedPlaceConcurrentlyWithoutFailing(@Mktmp final Path tmp) throws Exception {
        final Resolving resolving = new Resolving(
            null, tmp, (dep, place) -> { }, false, false, false, false,
            (Scalar<Dep>) () -> null, false
        );
        final Method cleaner = Resolving.class.getDeclaredMethod(
            "cleanPlace", Path.class, String.class, Set.class
        );
        cleaner.setAccessible(true);
        final int threads = 8;
        for (int trial = 0; trial < 20; ++trial) {
            final Path stale = tmp.resolve("stale-version");
            Files.createDirectories(stale.resolve("nested"));
            Files.createFile(stale.resolve("nested").resolve("file.txt"));
            final CountDownLatch ready = new CountDownLatch(threads);
            final CountDownLatch go = new CountDownLatch(1);
            final ExecutorService pool = Executors.newFixedThreadPool(threads);
            final List<Future<Object>> futures = new ArrayList<>(threads);
            try {
                final Callable<Object> job = () -> {
                    ready.countDown();
                    go.await();
                    return cleaner.invoke(
                        resolving, tmp, "1.0.0", new HashSet<>(Collections.emptySet())
                    );
                };
                for (int idx = 0; idx < threads; ++idx) {
                    futures.add(pool.submit(job));
                }
                ready.await();
                go.countDown();
                for (final Future<Object> future : futures) {
                    MatcherAssert.assertThat(
                        "cleanPlace must resolve to the requested version directory, but it didnt",
                        future.get(1, TimeUnit.MINUTES),
                        Matchers.equalTo(tmp.resolve("1.0.0"))
                    );
                }
            } finally {
                pool.shutdownNow();
            }
        }
    }
}
