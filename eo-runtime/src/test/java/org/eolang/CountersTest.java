/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Random;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Counters}.
 * @since 0.62
 */
final class CountersTest {

    @Test
    void countsAllocations() {
        final long seed = System.nanoTime();
        final int times = new Random(seed).nextInt(100) + 1;
        final Profile profile = new Counters();
        for (int idx = 0; idx < times; ++idx) {
            profile.allocate();
        }
        MatcherAssert.assertThat(
            String.format(
                "The profile counted the allocations wrong, %d of them were recorded, seed is %d",
                times, seed
            ),
            profile.allocations(),
            Matchers.equalTo((long) times)
        );
    }

    @Test
    void countsDispatchesApartFromAllocations() {
        final long seed = System.nanoTime();
        final int times = new Random(seed).nextInt(100) + 1;
        final Profile profile = new Counters();
        profile.allocate();
        for (int idx = 0; idx < times; ++idx) {
            profile.dispatch();
        }
        MatcherAssert.assertThat(
            String.format(
                "The profile mixed the dispatches with the allocations, %d of them were recorded, seed is %d",
                times, seed
            ),
            profile.dispatches(),
            Matchers.equalTo((long) times)
        );
    }
}
