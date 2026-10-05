/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.Together;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@code file.touched} under concurrency.
 *
 * <p>One caller creates and writes the file while the other callers touch the
 * same path. A losing exclusive open returns {@code EEXIST}; it must be
 * treated as a successful touch rather than as a failure from a later syscall
 * that overwrote {@code errno} (#7077).</p>
 *
 * <p>Two touchers, not seven: exactly one caller wins an exclusive create, so
 * a second toucher is all it takes to leave a loser, while every one of them
 * dataizes a whole object and {@link Maxmem} charges the lot to one test.
 * Seven of them ate more than the {@code eo.maxmem} of eo-runtime, so every
 * repetition was terminated and the test proved nothing at all on the box
 * slow enough to take that path (#8336).</p>
 *
 * <p>The directory is one {@link MktmpResolver} hands out, because it deletes
 * nothing. A test terminated for its memory is interrupted and reported while
 * the threads it started may still hold the file open, and a directory JUnit
 * owns is deleted the moment the test's context closes: on windows that
 * delete meets the open file, fails, and turns a skip into a broken
 * context.</p>
 *
 * @since 0.75.0
 */
@ExtendWith(MktmpResolver.class)
final class EOfileEOtouchedTest {

    @RepeatedTest(20)
    void keepsWhatAnotherThreadWroteInBetween(@Mktmp final Path temp)
        throws Exception {
        final Path target = temp.resolve("shared.txt");
        new Together<>(
            3,
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
