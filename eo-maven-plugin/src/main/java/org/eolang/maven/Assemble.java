/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;

/**
 * Assembles EO sources by running Parse, Probe, and Pull in a loop
 * until no new objects are discovered.
 *
 * @since 0.67.0
 */
final class Assemble {

    /**
     * Tojos to check assembly status.
     */
    private final TjsForeign tojos;

    /**
     * Parse step.
     */
    private final Parse parse;

    /**
     * Probe step.
     */
    private final Probe probe;

    /**
     * Pull step.
     */
    private final Pull pull;

    /**
     * Constructor.
     * @param tjs Foreign tojos
     * @param prs Parse step
     * @param prb Probe step
     * @param pll Pull step
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    Assemble(
        final TjsForeign tjs,
        final Parse prs,
        final Probe prb,
        final Pull pll
    ) {
        this.tojos = tjs;
        this.parse = prs;
        this.probe = prb;
        this.pull = pll;
    }

    /**
     * Run assembly cycles until stable.
     * @throws IOException If fails
     */
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void exec() throws IOException {
        final long begin = System.currentTimeMillis();
        String before = this.tojos.status();
        int cycle = 0;
        while (true) {
            final long start = System.currentTimeMillis();
            this.parse.exec();
            this.probe.exec();
            this.pull.exec();
            final String after = this.tojos.status();
            ++cycle;
            if (after.equals(before)) {
                Logger.info(
                    this, "Last assemble cycle #%d (%s), took %[ms]s",
                    cycle, after, System.currentTimeMillis() - start
                );
                break;
            } else {
                Logger.info(
                    this, "Assemble cycle #%d (%s -> %s), took %[ms]s",
                    cycle, before, after, System.currentTimeMillis() - start
                );
            }
            before = after;
        }
        Logger.info(
            this,
            "%d assemble cycle(s) produced some new object(s) in %[ms]s: %s",
            cycle,
            System.currentTimeMillis() - begin,
            before
        );
    }
}
