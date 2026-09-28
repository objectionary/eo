/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import java.util.concurrent.TimeUnit;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.PhApplication;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

/**
 * Test case for {@link EOscanf} diagnostics.
 *
 * <p>One dataization of a twenty-digit {@code %d} walks the recursive digit
 * fold and costs seconds, well over the {@code eo.deadline} an ordinary run
 * grants, so the cost is declared here instead of being discovered as a
 * skip. The memory it passes through the allocator is over
 * {@code eo.maxmem} too, and only the step of {@code fast.yml} that names
 * this class runs it with room for both.</p>
 *
 * @since 0.61.0
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@Timeout(value = 30L, unit = TimeUnit.SECONDS)
final class EOscanfDiagnosticTest {

    @Test
    void reportsSinglePercentForOversizedInteger() {
        final Phi scanf = new PhApplication(
            new Data.ToPhi("%d").take("scanf").copy(),
            "read", new Data.ToPhi("99999999999999999999")
        ).take("head");
        MatcherAssert.assertThat(
            "scanf must report the %d conversion with one percent sign",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(scanf).take()
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString(
                    "The number doesn't fit into long range for the '%d' conversion"
                ),
                Matchers.not(Matchers.containsString("%%d"))
            )
        );
    }
}
