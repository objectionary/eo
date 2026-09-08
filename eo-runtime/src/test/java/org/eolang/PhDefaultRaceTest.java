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
 *
 * @since 0.75.0
 */
final class PhDefaultRaceTest {

    @RepeatedTest(20)
    void appliesPositionallyWhileAttributesAreAdded() {
        final PhDefault phi = new PhDefault();
        phi.add("first", new AtVoid("first"));
        final Phi value = new Data.ToPhi(42L);
        new Together<>(
            16,
            thread -> {
                if (thread == 0) {
                    phi.put(0, value);
                } else {
                    final String name = String.format("a%d", thread);
                    phi.add(name, new AtVoid(name));
                }
                return true;
            }
        ).asList();
        MatcherAssert.assertThat(
            "a positional put must land on the attribute that was first when it was made, but it didnt",
            new Dataized(phi.take("first")).asNumber(),
            Matchers.equalTo(42.0d)
        );
    }

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
