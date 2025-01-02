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

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Test for {@link AtRho}.
 *
 * @since 0.50.0
 */
final class AtRhoTest {

    @ParameterizedTest
    @ValueSource(ints = {1, 4, 32})
    void worksAsExpected(final int threads) throws InterruptedException {
        final Attr rho = new AtRho();
        final CountDownLatch start = new CountDownLatch(1);
        final CountDownLatch end = new CountDownLatch(threads);
        final AtomicInteger count = new AtomicInteger(0);
        for (int idx = 0; idx < threads; ++idx) {
            final Thread thread = new Thread(
                () -> {
                    try {
                        start.await();
                        final Phi phi = new Phi.ToPhi(count.incrementAndGet());
                        rho.put(phi);
                    } catch (final InterruptedException ex) {
                        Thread.currentThread().interrupt();
                    } finally {
                        end.countDown();
                    }
                }
            );
            thread.start();
        }
        start.countDown();
        end.await();
        MatcherAssert.assertThat(
            "The object must be equal to 1",
            new BytesOf(rho.get().delta()).asNumber(),
            Matchers.equalTo(1.0)
        );
    }
}
