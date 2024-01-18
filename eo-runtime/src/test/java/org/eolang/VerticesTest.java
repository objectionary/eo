/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.Scalar;
import org.cactoos.experimental.Threads;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Vertices}.
 *
 * @since 0.18
 */
final class VerticesTest {

    @Test
    void makesNext() {
        MatcherAssert.assertThat(
            new Vertices().next(),
            Matchers.greaterThan(0)
        );
    }

    @Test
    void makesSameNumber() {
        final Vertices vtx = new Vertices();
        vtx.next();
        MatcherAssert.assertThat(
            vtx.best(1L),
            Matchers.equalTo(vtx.best(1L))
        );
    }

    /**
     * Test that {@link Vertices#best(Object)} and {@link Vertices#next()}
     * works correctly in multithreaded environment, i.e. produces non-repeatable
     * values.
     */
    @Test
    void performsVtxOperationsConcurrently() {
        final Set<Integer> hashes = ConcurrentHashMap.newKeySet();
        final Vertices vtx = new Vertices();
        final int threads = 1000;
        final List<Scalar<Integer>> tasks = new ArrayList<>(threads);
        tasks.addAll(
            Stream.generate(() -> (Scalar<Integer>) vtx::next)
                .limit(threads / 2)
                .collect(Collectors.toList())
        );
        tasks.addAll(
            Stream.generate(() -> (Scalar<Integer>) () -> vtx.best(new Random().nextLong()))
                .limit(threads / 2)
                .collect(Collectors.toList())
        );
        new Threads<>(
            threads,
            tasks
        ).forEach(hashes::add);
        MatcherAssert.assertThat(
            hashes.size(),
            Matchers.equalTo(threads)
        );
    }
}
