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

/**
 * Test case for {@code file.touched} under concurrency.
 *
 * <p>One caller creates and writes the file while the other callers touch the
 * same path. A losing exclusive open returns {@code EEXIST}; it must be
 * treated as a successful touch rather than as a failure from a later syscall
 * that overwrote {@code errno} (#7077).</p>
 *
 * <p>The temporary directory is kept when the test does not pass. A
 * memory or deadline abort leaves the threads of this test still
 * holding files open under it, and Windows refuses to delete a
 * directory whose files are open, so an unconditional cleanup turns
 * the abort JUnit intended as a skip into a failure of the run
 * (#8325).</p>
 *
 * @since 0.75.0
 */
final class EOfileEOtouchedTest {

    @RepeatedTest(20)
    void keepsWhatAnotherThreadWroteInBetween(@TempDir(cleanup = CleanupMode.ON_SUCCESS) final Path temp)
        throws Exception {
        final Path target = temp.resolve("shared.txt");
        new Together<>(
            8,
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
