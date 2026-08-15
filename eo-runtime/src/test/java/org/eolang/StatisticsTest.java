/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Statistics}, the one an object is handed when it is
 * made and reports its births and dispatches to.
 * @since 0.62
 */
final class StatisticsTest {

    @Test
    void countsOneAllocationPerObjectComingAlive() {
        final Statistics stats = new Counters();
        final Phi phi = new PhDefault(stats, "org.eolang.bytes");
        phi.hasRho();
        phi.hasRho();
        MatcherAssert.assertThat(
            "an object came alive as other than exactly one allocation",
            stats.allocations(),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void countsOneAllocationPerCopy() {
        final Statistics stats = new Counters();
        final Phi phi = new PhDefault(stats, "org.eolang.bytes");
        phi.hasRho();
        phi.copy();
        MatcherAssert.assertThat(
            "copying one object moved the statistics by other than one allocation",
            stats.allocations(),
            Matchers.equalTo(2L)
        );
    }

    @Test
    void countsOneDispatchPerTake() {
        final Statistics stats = new Counters();
        final Phi phi = new PhDefault(stats, new Attrs(new Attr("x", new AtVoid("x"))));
        phi.take("x");
        MatcherAssert.assertThat(
            "taking one attribute moved the statistics by other than one dispatch",
            stats.dispatches(),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void keepsTheStatisticsOfTwoProgramsApart() {
        final Statistics mine = new Counters();
        final Statistics yours = new Counters();
        new PhDefault(mine, new Attrs(new Attr("x", new AtVoid("x")))).take("x");
        MatcherAssert.assertThat(
            "an object born and dispatched in one program was counted in another",
            yours.allocations() + yours.dispatches(),
            Matchers.equalTo(0L)
        );
    }
}
