/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.security.SecureRandom;
import java.util.Random;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Counters}.
 * @since 0.73.3
 */
final class CountersTest {

    @Test
    void countsEveryBirth() {
        final long seed = new SecureRandom().nextLong();
        final int births = 1 + new Random(seed).nextInt(1000);
        final Statistics stats = new Counters();
        for (int idx = 0; idx < births; ++idx) {
            stats.allocate();
        }
        MatcherAssert.assertThat(
            String.format("counted births are not the ones recorded, seed is %d", seed),
            stats.allocations(),
            Matchers.equalTo((long) births)
        );
    }

    @Test
    void countsEveryLookup() {
        final long seed = new SecureRandom().nextLong();
        final int lookups = 1 + new Random(seed).nextInt(1000);
        final Statistics stats = new Counters();
        for (int idx = 0; idx < lookups; ++idx) {
            stats.dispatch();
        }
        MatcherAssert.assertThat(
            String.format("counted lookups are not the ones recorded, seed is %d", seed),
            stats.dispatches(),
            Matchers.equalTo((long) lookups)
        );
    }
}
