/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests of the class {@link Uses}.
 *
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
final class UsesTest {

    @Test
    void givesEveryThreadTheHashItGetsAlone(@Mktmp final Path temp)
        throws IOException, InterruptedException, ExecutionException {
        final int count = 64;
        Files.write(
            temp.resolve("entries.xmir"),
            IntStream.rangeClosed(1, count)
                .mapToObj(n -> String.format("<o name='e%d'><o base='Φ.e%d'/></o>", n, n))
                .collect(
                    Collectors.joining(
                        "", "<object><o><o name='mark'/><o name='root'/>", "</o></object>"
                    )
                )
                .getBytes(StandardCharsets.UTF_8)
        );
        final List<String> alone = new ArrayList<>(count);
        final Uses single = new Uses(temp);
        for (int num = 1; num <= count; ++num) {
            alone.add(single.hash(num, String.format("Φ.e%d", num)));
        }
        final Uses shared = new Uses(temp);
        final ExecutorService pool = Executors.newFixedThreadPool(32);
        try {
            final List<Callable<String>> tasks = new ArrayList<>(count);
            for (int num = 1; num <= count; ++num) {
                final int entry = num;
                tasks.add(() -> shared.hash(entry, String.format("Φ.e%d", entry)));
            }
            final List<String> together = new ArrayList<>(count);
            for (final Future<String> future : pool.invokeAll(tasks)) {
                together.add(future.get());
            }
            MatcherAssert.assertThat(
                "every thread must get the hash its entry gets alone, but some didnt",
                together,
                Matchers.equalTo(alone)
            );
        } finally {
            pool.shutdownNow();
        }
    }
}
