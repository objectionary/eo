/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.yegor256.Together;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.io.CleanupMode;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for {@code file.touched} under concurrency.
 *
 * <p>One caller creates and writes the file while the other callers touch the
 * same path. A losing exclusive open returns {@code EEXIST}; it must be
 * treated as a successful touch rather than as a failure from a later syscall
 * that overwrote {@code errno} (#7077).</p>
 *
 * <p>Four threads, and the repetitions one after another rather than beside
 * each other, because of what the losing branch costs: it asks
 * {@code file.is-symlink}, which is tens of megabytes of allocation, so
 * eight of those threads, in twenty bunches at once, allocate several times
 * the {@code eo.maxmem} a single repetition is given, and {@link Maxmem}
 * terminates the repetition instead of letting it finish. Three touching
 * threads still leave somebody holding the {@code EEXIST}, which is the
 * branch this reaches for.</p>
 *
 * <p>The temporary directory is kept when a repetition does not succeed,
 * since a repetition that is terminated has threads of its own still on the
 * way out: they write the file back while JUnit walks the directory to
 * delete it, the walk ends on a directory that refuses to be empty, and the
 * skip is reported as an error of the test.</p>
 *
 * @since 0.75.0
 */
@Execution(ExecutionMode.SAME_THREAD)
final class EOfileEOtouchedTest {

    @RepeatedTest(20)
    void keepsWhatAnotherThreadWroteInBetween(
        @TempDir(cleanup = CleanupMode.ON_SUCCESS) final Path temp
    ) throws Exception {
        final Path target = temp.resolve("shared.txt");
        new Together<>(
            4,
            thread -> {
                if (thread == 0) {
                    Files.writeString(target, "content", StandardCharsets.UTF_8);
                } else {
                    new Dataized(this.touched(target)).take();
                }
                return target;
            }
        ).asList();
        MatcherAssert.assertThat(
            "a touch that loses the creation race must not erase the other writer's data",
            Files.readString(target, StandardCharsets.UTF_8),
            Matchers.equalTo("content")
        );
    }

    private Phi touched(final Path target) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(target.toString()));
        return file.take("touched");
    }
}
