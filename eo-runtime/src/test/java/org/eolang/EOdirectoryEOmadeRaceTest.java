/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.yegor256.Together;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@code directory.made} under concurrency.
 *
 * <p>{@code made} asks whether the path is there and makes it when it is not,
 * and the two questions are not one step: another thread can make the very
 * same directory in between, so the {@code mkdir} of this one comes back with
 * {@code EEXIST} having done nothing wrong. Answering that with the directory
 * rather than with a failure is the branch this exercises, and a single-threaded
 * {@code ++>} test cannot reach it, since nothing there ever loses the race
 * (#7080).</p>
 *
 * <p>The interleaving is left to the threads instead of being forced between
 * the two steps: every thread is given the same missing path, so whichever one
 * wins the {@code mkdir} leaves the rest to take the {@code EEXIST} branch, and
 * a deep path gives them several levels to collide on. A thread that is refused
 * comes back through {@code Together} as the failure it threw.</p>
 *
 * <p>Four threads are enough to leave somebody holding the {@code EEXIST}, and
 * every thread of them builds a graph of objects of its own: a dozen at once is
 * gigabytes of heap and minutes of collecting it, for no more of the race than
 * four reach.</p>
 *
 * @since 0.75.0
 */
final class EOdirectoryEOmadeRaceTest {

    @RepeatedTest(10)
    void makesOneDirectoryFromManyThreadsAtOnce(@TempDir final Path temp) {
        final Path target = temp.resolve("one").resolve("two").resolve("three");
        final int threads = 4;
        final List<Boolean> outcomes = new Together<>(
            threads,
            thread -> new Dataized(
                this.made(target.toString()).take("exists")
            ).asBool()
        ).asList();
        MatcherAssert.assertThat(
            "every thread racing for the same missing directory must be told it is there, since the one that loses the mkdir is the one the EEXIST branch rescues",
            outcomes,
            Matchers.everyItem(Matchers.is(true))
        );
        MatcherAssert.assertThat(
            "the directory the threads raced for must be on disk once they are done",
            Files.isDirectory(target),
            Matchers.is(true)
        );
    }

    private Phi made(final String path) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(path));
        final Phi directory = Phi.Φ.take("directory").copy();
        directory.put(0, file);
        return directory.take("made");
    }
}
