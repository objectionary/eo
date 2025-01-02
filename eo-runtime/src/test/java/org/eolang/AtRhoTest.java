package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;

final class AtRhoTest {

    @ParameterizedTest
    @ValueSource(ints = {1, 4, 32})
    void threadSafeAtRho(final int threads) throws InterruptedException {
        final Attr atRho = new AtRho();
        final CountDownLatch startLatch = new CountDownLatch(1);
        final CountDownLatch endLatch = new CountDownLatch(threads);
        final AtomicInteger count = new AtomicInteger(0);
        for (int i = 0; i < threads; i++) {
            new Thread(() -> {
                try {
                    startLatch.await();
                    atRho.put(new Phi.ToPhi(count.incrementAndGet()));
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                } finally {
                    endLatch.countDown();
                }
            }).start();
        }
        startLatch.countDown();
        endLatch.await();
        MatcherAssert.assertThat(
                "The object must be equal to 1",
                new BytesOf(
                        atRho.get().delta()
                ).asNumber(),
                Matchers.equalTo(1.0)
        );
    }
}
