/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.UnaryOperator;
import org.eolang.lints.Severity;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link Linting}.
 * @since 0.31.0
 */
@ExtendWith(MktmpResolver.class)
final class LintingTest {

    @Test
    void skipsLintingWhenFlagIsSet(@TempDir final Path temp) {
        final TjsForeign tojos = new TjsForeign();
        Assertions.assertDoesNotThrow(
            () -> new Linting(
                tojos,
                tojos,
                temp,
                temp,
                false,
                "0.0.0",
                Collections.emptyList(),
                Collections.emptyList(),
                false,
                false,
                false,
                temp,
                true
            ).exec(),
            "Linting must be fully skipped when skipLinting is TRUE"
        );
    }

    /**
     * Race threads of one {@link Linting} on a single tail path (#5720): the
     * first one lints, the rest wait for its lock and read what it cached.
     * With a guard per file each thread locked its own empty map, so every one
     * of them missed and the same XMIR was linted once per thread. The latch
     * makes that deterministic rather than timing-dependent: a shared guard
     * lets exactly one thread in, so it waits out the timeout alone, while a
     * guard per file lets all eight in at once and the latch opens at once.
     * Without it the linting here is too quick to ever overlap, and the test
     * passes against the broken code too. The target is read back after the
     * guard is released, where {@link Saved} can be caught mid-write (#5873),
     * which is a different race and is ignored here, as in {@link ParsingTest}.
     * @param temp Temporary directory
     * @throws Exception If fails
     */
    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void lintsOnlyOnceWhenThreadsRaceOnTheSameTail(@Mktmp final Path temp) throws Exception {
        final TjsForeign tojos = new FakeMaven(temp)
            .withProgram("+package foo", "", "# Sample.", "[] > main")
            .execute(MjParse.class)
            .foreignTojos();
        final TjForeign tojo = tojos.withXmir().iterator().next();
        final Linting linting = new Linting(
            tojos, tojos, temp.resolve("linted"), temp.resolve("lint-cache"), true, "0.0.0",
            Collections.emptyList(), Collections.emptyList(), false, false, false, temp, false
        );
        final Map<Severity, Integer> counts = new ConcurrentHashMap<>();
        for (final Severity severity : Severity.values()) {
            counts.putIfAbsent(severity, 0);
        }
        final Collection<String> seen = new ConcurrentLinkedQueue<>();
        final AtomicInteger lints = new AtomicInteger(0);
        final CountDownLatch entered = new CountDownLatch(8);
        final UnaryOperator<XML> pipeline = xmir -> {
            lints.incrementAndGet();
            entered.countDown();
            try {
                entered.await(1L, TimeUnit.SECONDS);
            } catch (final InterruptedException interrupted) {
                Thread.currentThread().interrupt();
            }
            return xmir;
        };
        new Threaded<>(
            Collections.nCopies(8, tojo),
            each -> {
                try {
                    linting.lintOne(each, counts, seen, pipeline);
                } catch (final IllegalArgumentException half) {
                    Logger.debug(this, "Half-written XMIR (#5873): %s", half);
                }
                return 1;
            }
        ).total();
        MatcherAssert.assertThat(
            "Threads racing on one tail path must lint the XMIR once, the rest must be cached",
            lints.get(),
            Matchers.equalTo(1)
        );
    }
}
