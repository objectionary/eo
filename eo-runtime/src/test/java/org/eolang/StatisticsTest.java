/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Isolated;

/**
 * Test case for {@link Statistics}, the running one that {@link PhDefault}
 * feeds. The class runs alone, since the statistics it reads are shared by
 * every object in the JVM.
 * @since 0.62
 */
@Isolated
final class StatisticsTest {

    @Test
    void countsOneAllocationPerObjectComingAlive() {
        final Phi phi = new PhDefault("org.eolang.bytes");
        final long before = Statistics.RUNNING.allocations();
        phi.hasRho();
        phi.hasRho();
        MatcherAssert.assertThat(
            "an object came alive as other than exactly one allocation",
            Statistics.RUNNING.allocations() - before,
            Matchers.equalTo(1L)
        );
    }

    @Test
    void countsOneAllocationPerCopy() {
        final Phi phi = new PhDefault("org.eolang.bytes");
        phi.hasRho();
        final long before = Statistics.RUNNING.allocations();
        phi.copy();
        MatcherAssert.assertThat(
            "copying one object moved the statistics by other than one allocation",
            Statistics.RUNNING.allocations() - before,
            Matchers.equalTo(1L)
        );
    }

    @Test
    void countsOneDispatchPerTake() {
        final Phi phi = new PhDefault(new Attrs(new Attr("x", new AtVoid("x"))));
        final long before = Statistics.RUNNING.dispatches();
        phi.take("x");
        MatcherAssert.assertThat(
            "taking one attribute moved the statistics by other than one dispatch",
            Statistics.RUNNING.dispatches() - before,
            Matchers.equalTo(1L)
        );
    }
}
