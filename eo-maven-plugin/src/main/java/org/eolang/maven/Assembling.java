/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;

/**
 * Assembles EO sources by running Parsing, Probing, and Pulling in a loop
 * until no new objects are discovered.
 * @since 0.61.0
 */
final class Assembling implements Step {

    /**
     * Tojos to check assembly status.
     */
    private final TjsForeign tojos;

    /**
     * Parse step.
     */
    private final Step parse;

    /**
     * Probe step.
     */
    private final Step probe;

    /**
     * Pull step.
     */
    private final Step pull;

    /**
     * Constructor.
     * @param tjs Foreign tojos
     * @param prs Parse step
     * @param prb Probe step
     * @param pll Pull step
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    Assembling(
        final TjsForeign tjs,
        final Step prs,
        final Step prb,
        final Step pll
    ) {
        this.tojos = tjs;
        this.parse = prs;
        this.probe = prb;
        this.pull = pll;
    }

    @Override
    public void exec() throws IOException {
        String before = this.tojos.status();
        int cycle = 0;
        while (true) {
            this.parse.exec();
            this.probe.exec();
            this.pull.exec();
            final String after = this.tojos.status();
            ++cycle;
            if (after.equals(before)) {
                Logger.info(
                    this, "Last assemble cycle #%d (%s)",
                    cycle, after
                );
                break;
            } else {
                Logger.info(
                    this, "Assemble cycle #%d (%s -> %s)",
                    cycle, before, after
                );
            }
            before = after;
        }
        Logger.info(
            this,
            "%d assemble cycle(s) produced some new object(s): %s",
            cycle,
            before
        );
    }
}
