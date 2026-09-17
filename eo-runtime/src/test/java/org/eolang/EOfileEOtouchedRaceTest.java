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
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@code file.touched} under concurrency.
 *
 * <p>{@code touched} asks whether the path can be reached and creates it when
 * it cannot, and the two questions are not one step: another thread can create
 * the very same file in between, so the exclusive {@code open} of this one
 * comes back with {@code EEXIST} having done nothing wrong. Answering that
 * with the file rather than with a failure is the branch this exercises, and a
 * single-threaded {@code ++>} test cannot reach it, since nothing there ever
 * loses the race (#7077).</p>
 *
 * <p>The interleaving is left to the threads instead of being forced between
 * the two steps: every thread is given the same missing path, so whichever one
 * wins the {@code open} leaves the rest to take the {@code EEXIST} branch. A
 * thread that is refused comes back through {@code Together} as the failure it
 * threw, and one that is answered reads {@code exists} off what it was given,
 * which is the file on disk and not a claim about it.</p>
 *
 * <p>Four threads are enough to leave somebody holding the {@code EEXIST}, and
 * every thread of them builds a graph of objects of its own: a dozen at once is
 * gigabytes of heap and minutes of collecting it, for no more of the race than
 * four reach.</p>
 *
 * @since 0.75.0
 */
final class EOfileEOtouchedRaceTest {

    /**
     * What the writing thread puts in the raced file.
     */
    private static final String CONTENT = "written by somebody else";

    @RepeatedTest(10)
    void touchesOneFileFromManyThreadsAtOnce(@TempDir final Path temp) {
        final String path = temp.resolve("touched.txt").toString();
        MatcherAssert.assertThat(
            "every thread racing for the same missing file must be told it is there, since the one that loses the exclusive open is the one the EEXIST branch rescues",
            new Together<>(
                4,
                thread -> new Dataized(
                    this.touched(path).take("exists")
                ).asBool()
            ).asList(),
            Matchers.everyItem(Matchers.is(true))
        );
    }

    @RepeatedTest(10)
    void keepsWhatAnotherThreadWroteInBetween(@TempDir final Path temp) throws Exception {
        final Path file = temp.resolve("shared.txt");
        new Together<>(
            4,
            thread -> {
                final boolean done;
                if (thread == 0) {
                    Files.write(
                        file, EOfileEOtouchedRaceTest.CONTENT.getBytes(StandardCharsets.UTF_8)
                    );
                    done = true;
                } else {
                    done = new Dataized(
                        this.touched(file.toString()).take("exists")
                    ).asBool();
                }
                return done;
            }
        ).asList();
        MatcherAssert.assertThat(
            "touching a file another thread has just written must hand it back unchanged, since nothing here opens it with O_TRUNC",
            new String(Files.readAllBytes(file), StandardCharsets.UTF_8),
            Matchers.equalTo(EOfileEOtouchedRaceTest.CONTENT)
        );
    }

    private Phi touched(final String path) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(path));
        return file.take("touched");
    }
}
