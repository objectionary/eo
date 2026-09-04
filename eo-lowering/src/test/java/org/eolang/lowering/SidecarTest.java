/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link Sidecar}.
 * @since 0.76.0
 */
final class SidecarTest {

    @Test
    void savesBodyUnderItsDigestName(@TempDir final Path temp) throws Exception {
        final String body = "return new Data.ToPhi(42.17d);";
        MatcherAssert.assertThat(
            "the sidecar didnt keep the whole body under its digest name",
            Files.readString(
                temp.resolve(
                    String.format("%s.java", new Sidecar(temp, body).save())
                )
            ),
            Matchers.equalTo(body)
        );
    }

    @Test
    void leavesNothingButTheSidecarBehind(@TempDir final Path temp) throws Exception {
        new Sidecar(temp, "final double s1 = v0 * v0;").save();
        final long count;
        try (Stream<Path> kids = Files.list(temp)) {
            count = kids.count();
        }
        MatcherAssert.assertThat(
            "the temporary file didnt disappear after the save",
            count,
            Matchers.equalTo(1L)
        );
    }

    @Test
    void survivesRacingSaversOfOneBody(@TempDir final Path temp) throws Exception {
        final String body = String.join(
            System.lineSeparator(),
            "final double s1 = v0 + v1;",
            "return new Data.ToPhi(s1);"
        );
        final int savers = 16;
        final CountDownLatch start = new CountDownLatch(1);
        final Collection<FutureTask<String>> tasks = new ArrayList<>(savers);
        for (int idx = 0; idx < savers; ++idx) {
            final FutureTask<String> task = new FutureTask<>(
                () -> {
                    start.await(10L, TimeUnit.SECONDS);
                    return new Sidecar(temp, body).save();
                }
            );
            tasks.add(task);
            new Thread(task).start();
        }
        start.countDown();
        for (final FutureTask<String> task : tasks) {
            task.get(30L, TimeUnit.SECONDS);
        }
        MatcherAssert.assertThat(
            "a racing save left the sidecar truncated or torn",
            Files.readString(
                temp.resolve(String.format("%s.java", new Digest(body).hex()))
            ),
            Matchers.equalTo(body)
        );
    }
}
