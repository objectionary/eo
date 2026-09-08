/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
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
