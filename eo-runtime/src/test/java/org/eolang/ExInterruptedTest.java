/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.time.Duration;
import java.util.concurrent.atomic.AtomicReference;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link ExInterrupted}.
 *
 * @since 0.74.0
 */
final class ExInterruptedTest {

    @Test
    void stopsTakingInInterruptedThread() throws InterruptedException {
        final PhDefault phi = new PhDefault();
        phi.add("stopsTakingInInterruptedThread", new AtComposite(phi, rho -> new Data.ToPhi(42L)));
        final AtomicReference<Throwable> thrown = new AtomicReference<>();
        final Thread thread = new Thread(
            () -> {
                while (true) {
                    phi.take("stopsTakingInInterruptedThread");
                }
            }
        );
        thread.setDaemon(true);
        thread.setUncaughtExceptionHandler((ignore, error) -> thrown.set(error));
        thread.start();
        thread.interrupt();
        thread.join(Duration.ofSeconds(10L).toMillis());
        MatcherAssert.assertThat(
            "Interrupted thread must abandon its computation, but it kept taking attributes",
            thrown.get(),
            Matchers.instanceOf(ExInterrupted.class)
        );
    }
}
