/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.yegor256.Together;
import java.util.stream.IntStream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.RepeatedTest;

/**
 * Test case for {@link PhDefault}'s lazy attribute loading under concurrency.
 * @since 0.75.0
 */
final class PhDefaultRaceTest {

    @RepeatedTest(20)
    void loadsAttributesOnceUnderConcurrentTake() {
        final int threads = 32;
        final Phi phi = new PhDefault(
            new Attrs(
                IntStream.range(0, 8)
                    .mapToObj(idx -> new Attr(String.format("a%d", idx), new AtVoid("void")))
                    .toArray(Attr[]::new)
            )
        );
        MatcherAssert.assertThat(
            "taking attrs of one un-warmed object from many threads at once must not corrupt it",
            new Together<>(threads, t -> phi.take("a0")),
            Matchers.iterableWithSize(threads)
        );
    }
}
