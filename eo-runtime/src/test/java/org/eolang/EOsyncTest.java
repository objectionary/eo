/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.LockSupport;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOsync}.
 * @since 0.74.0
 */
final class EOsyncTest {

    @Test
    void letsOnlyOneThreadDispatchOnTheDecoratedObject() throws InterruptedException {
        final AtomicInteger inside = new AtomicInteger(0);
        final AtomicInteger peak = new AtomicInteger(0);
        final PhDefault greeting = new PhDefault();
        greeting.add(
            "name",
            new AtComposite(
                greeting,
                rho -> {
                    peak.accumulateAndGet(inside.incrementAndGet(), Math::max);
                    LockSupport.parkNanos(1_000_000L);
                    inside.decrementAndGet();
                    return new Data.ToPhi("Привет, друг!");
                }
            )
        );
        final Phi guarded = new EOsync();
        guarded.put(0, greeting);
        final Thread[] pool = new Thread[8];
        for (int idx = 0; idx < pool.length; idx += 1) {
            pool[idx] = new Thread(() -> guarded.take("name"));
            pool[idx].start();
        }
        for (final Thread thread : pool) {
            thread.join(30_000L);
        }
        MatcherAssert.assertThat(
            "sync let more than one thread into the object it decorates at once",
            peak.get(),
            Matchers.equalTo(1)
        );
    }
}
