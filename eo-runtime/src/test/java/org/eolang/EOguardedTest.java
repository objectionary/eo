/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOguarded}.
 * @since 0.74.0
 */
final class EOguardedTest {

    @Test
    void runsCleanupAfterSuccessfulScope() {
        final AtomicInteger runs = new AtomicInteger(0);
        new Dataized(EOguardedTest.guarded(new Data.ToPhi(true), runs)).take();
        MatcherAssert.assertThat(
            "cleanup must run exactly once after a successful scope, but it didn't",
            runs.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void runsCleanupAfterFailingScope() {
        final AtomicInteger runs = new AtomicInteger(0);
        final Phi failing = new PhDefault();
        failing.add(
            Phi.PHI,
            new AtComposite(
                failing,
                rho -> {
                    throw new ExFailure("scope failed on purpose");
                }
            )
        );
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(EOguardedTest.guarded(failing, runs)).take(),
            "a failing scope must still propagate, but it didn't"
        );
        MatcherAssert.assertThat(
            "cleanup must run exactly once after a failing scope, but it didn't",
            runs.get(),
            Matchers.equalTo(1)
        );
    }

    /**
     * A guarded object with a counting cleanup.
     * @param scope The scope
     * @param runs Cleanup call counter
     * @return Guarded object
     */
    private static Phi guarded(final Phi scope, final AtomicInteger runs) {
        final Phi cleanup = new PhDefault();
        cleanup.add(
            Phi.PHI,
            new AtComposite(
                cleanup,
                rho -> {
                    runs.incrementAndGet();
                    return new Data.ToPhi(true);
                }
            )
        );
        final Phi phi = new EOguarded();
        phi.put("scope", scope);
        phi.put("cleanup", cleanup);
        return phi;
    }
}
