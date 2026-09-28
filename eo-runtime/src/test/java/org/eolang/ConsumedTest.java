/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.CountDownLatch;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Consumed}.
 *
 * @since 0.75.0
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "PMD.AvoidThreadGroup"})
final class ConsumedTest {

    @Test
    void countsBytesOfThreadThatIsAlreadyGone() throws InterruptedException {
        final int chunk = 8 * 1024 * 1024;
        MatcherAssert.assertThat(
            "The bytes of a thread must be counted while it lives and stay in the tally after it is gone, but they didnt",
            ConsumedTest.sampled(chunk),
            Matchers.greaterThanOrEqualTo((long) chunk)
        );
    }

    @Test
    void countsNothingForEmptyGroup() {
        MatcherAssert.assertThat(
            "A group with no threads in it must have allocated nothing, but it didnt",
            new Consumed(new ThreadGroup("consumed-empty")).bytes(),
            Matchers.equalTo(0L)
        );
    }

    private static long sampled(final int chunk) throws InterruptedException {
        final ThreadGroup group = new ThreadGroup("consumed-counts");
        final Consumed consumed = new Consumed(group);
        final CountDownLatch taken = new CountDownLatch(1);
        final CountDownLatch counted = new CountDownLatch(1);
        final byte[][] junk = new byte[1][];
        final Thread thread = new Thread(
            group,
            () -> {
                junk[0] = new byte[chunk];
                taken.countDown();
                try {
                    counted.await();
                } catch (final InterruptedException ex) {
                    Thread.currentThread().interrupt();
                }
            }
        );
        thread.setDaemon(true);
        thread.start();
        taken.await();
        consumed.refresh();
        counted.countDown();
        thread.join();
        return consumed.bytes();
    }
}
