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
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Saved}.
 * @since 0.74
 */
@ExtendWith(MktmpResolver.class)
final class SavedTest {

    @Test
    void savesContentToFile(@Mktmp final Path temp) throws IOException {
        final Path target = temp.resolve("out.txt");
        new Saved("hello", target).value();
        MatcherAssert.assertThat(
            "the saved file must contain exactly what was written",
            Files.readString(target, StandardCharsets.UTF_8),
            Matchers.equalTo("hello")
        );
    }

    @Test
    void exposesNoPartiallyWrittenFileToConcurrentReader(
        @Mktmp final Path temp
    ) throws Exception {
        final Path target = temp.resolve("shared.txt");
        final List<String> variants = IntStream.range(0, 8)
            .mapToObj(idx -> String.valueOf((char) ('a' + idx)).repeat(500_000))
            .collect(Collectors.toList());
        new Saved(variants.get(0), target).value();
        final List<String> broken = new CopyOnWriteArrayList<>();
        new Threaded<>(
            variants,
            variant -> {
                new Saved(variant, target).value();
                final String read = Files.readString(target, StandardCharsets.UTF_8);
                if (!variants.contains(read)) {
                    broken.add(read.substring(0, Math.min(read.length(), 40)));
                }
                return 1;
            }
        ).total();
        MatcherAssert.assertThat(
            "a reader racing with concurrent writers must always see a complete variant, never a mix of two",
            broken,
            Matchers.empty()
        );
    }

    @Test
    void retriesMoveWhenTargetTemporarilyBlocked(@Mktmp final Path temp) throws Exception {
        final Path target = temp.resolve("blocked.txt");
        Files.createDirectories(target);
        try (ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor()) {
            scheduler.schedule(
                (Callable<Void>) () -> {
                    Files.delete(target);
                    return null;
                }, 300, TimeUnit.MILLISECONDS
            );
            new Saved("content", target).value();
        }
        MatcherAssert.assertThat(
            "the write must succeed once a retry lands after the obstruction clears",
            Files.readString(target, StandardCharsets.UTF_8),
            Matchers.equalTo("content")
        );
    }
}
