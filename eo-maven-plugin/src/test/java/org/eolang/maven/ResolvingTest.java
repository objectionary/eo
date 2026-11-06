/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.Scalar;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Mapped;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Resolving}.
 *
 * @since 0.61.0
 */
@ExtendWith(MktmpResolver.class)
final class ResolvingTest {

    @Test
    void retriesFailedDependencyAndMarksEveryRelatedTojo(@Mktmp final Path tmp) throws Exception {
        final Path xmir = tmp.resolve("library.xmir");
        Files.writeString(
            xmir,
            String.join(
                "", "<object><metas><meta><head>rt</head><part>jvm</part>",
                "<part>org.example:library:1.0.0</part></meta></metas></object>"
            )
        );
        try (TjsForeign tojos = new TjsForeign()) {
            tojos.add("first").withXmir(xmir).withVersion("1.0.0");
            tojos.add("second").withXmir(xmir).withVersion("1.0.0");
            final AtomicInteger attempts = new AtomicInteger();
            final Resolving resolving = new Resolving(
                tojos, tmp.resolve("target"),
                (dep, place) -> {
                    if (attempts.incrementAndGet() == 1) {
                        throw new IllegalStateException("The first download failed");
                    }
                    try {
                        Files.createDirectories(place);
                        Files.writeString(place.resolve("library.class"), "downloaded");
                    } catch (final IOException err) {
                        throw new UncheckedIOException(err);
                    }
                },
                false, false, false, true, (Scalar<Dep>) () -> null, false
            );
            final List<Integer> observed = new ArrayList<>(5);
            int failed = 0;
            try {
                resolving.exec();
            } catch (final UncheckedIOException err) {
                failed = 1;
            }
            observed.add(failed);
            observed.add(tojos.dependencies().size());
            resolving.exec();
            observed.add(attempts.get());
            observed.add(tojos.dependencies().size());
            resolving.exec();
            observed.add(attempts.get());
            MatcherAssert.assertThat(
                "A failed download must keep both tojos pending until a successful retry",
                observed, Matchers.equalTo(List.of(1, 2, 2, 0, 2))
            );
        }
    }

    @Test
    void cleansUpSharedPlaceConcurrentlyWithoutFailing(@Mktmp final Path tmp) throws Exception {
        final Resolving resolving = new Resolving(
            null, tmp, (dep, place) -> { }, false, false, false, false,
            (Scalar<Dep>) () -> null, false
        );
        final int threads = 8;
        final List<Integer> slots = new ArrayList<>(threads);
        for (int idx = 0; idx < threads; ++idx) {
            slots.add(idx);
        }
        for (int trial = 0; trial < 20; ++trial) {
            final Path stale = tmp.resolve("stale-version");
            Files.createDirectories(stale.resolve("nested"));
            Files.write(
                stale.resolve("nested").resolve("file.txt"),
                "stale".getBytes(StandardCharsets.UTF_8)
            );
            for (final Path place : new Threads<Path>(
                threads,
                new Mapped<Scalar<Path>>(
                    idx -> () -> resolving.cleanPlace(
                        tmp, "1.0.0", new HashSet<>(Collections.emptySet())
                    ),
                    slots
                )
            )) {
                MatcherAssert.assertThat(
                    "cleanPlace must resolve to the requested version directory, but it didnt",
                    place,
                    Matchers.equalTo(tmp.resolve("1.0.0"))
                );
            }
        }
    }
}
