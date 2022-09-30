/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
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
            Matchers.equalTo(1)
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

    @Test
    void vtxThreadTest() throws InterruptedException {
        final int threads = 8;
        final Set<Integer> hashes = ConcurrentHashMap.newKeySet();
        final ExecutorService executor = Executors.newFixedThreadPool(threads);
        final CountDownLatch latch = new CountDownLatch(1);
        final Vertices vtx = new Vertices();
        for (long index = 0; index < threads; ++index) {
            final long thread = index;
            executor.submit(
                () -> {
                    latch.await();
                    final int hash = vtx.best(thread);
                    hashes.add(hash);
                    return hash;
                }
            );
        }
        latch.countDown();
        executor.awaitTermination(1, TimeUnit.SECONDS);
        MatcherAssert.assertThat(
            hashes.size(),
            Matchers.equalTo(threads)
        );
    }
}
