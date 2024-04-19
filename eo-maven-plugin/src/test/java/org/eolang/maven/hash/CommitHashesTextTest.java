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
package org.eolang.maven.hash;

import com.yegor256.WeAreOnline;
import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link CommitHashesText}.
 *
 * @since 0.37.0
 */
final class CommitHashesTextTest {

    @Test
    @ExtendWith(WeAreOnline.class)
    void downloadsDefaultList() throws Exception {
        MatcherAssert.assertThat(
            "CommitHashesText downloads the default list of hashes from Objectionary",
            new CommitHashesText().asString(),
            Matchers.containsString("master")
        );
    }

    @Test
    void isThreadSafe() throws ExecutionException, InterruptedException, TimeoutException {
        final int threads = 200;
        boolean nonulls = true;
        final ExecutorService service = Executors.newFixedThreadPool(threads);
        final CountDownLatch latch = new CountDownLatch(1);
        final Collection<Future<Boolean>> futures = new ArrayList<>(threads);
        try {
            for (int thread = 0; thread < threads; ++thread) {
                futures.add(
                    service.submit(
                        () -> new CommitHashesText().asString() != null
                    )
                );
            }
            latch.countDown();
            for (final Future<Boolean> fun : futures) {
                nonulls &= fun.get(1, TimeUnit.SECONDS);
            }
            MatcherAssert.assertThat(
                "Can be used in different threads without NPE",
                nonulls,
                Matchers.equalTo(true)
            );
        } finally {
            service.shutdown();
        }
    }
}
