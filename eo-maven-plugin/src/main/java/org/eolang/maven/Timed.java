/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;

/**
 * A decorator that measures and logs the execution time of a {@link Step}.
 * @since 0.68.0
 */
final class Timed implements Step {

    /**
     * The step to measure.
     */
    private final Step origin;

    /**
     * Label for logging.
     */
    private final String label;

    /**
     * Constructor.
     * @param step The step to wrap
     * @param lbl Label to use in log message
     */
    Timed(final Step step, final String lbl) {
        this.origin = step;
        this.label = lbl;
    }

    @Override
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    public void exec() throws IOException {
        final long start = System.currentTimeMillis();
        this.origin.exec();
        Logger.info(
            this,
            "%s took %[ms]s",
            this.label,
            System.currentTimeMillis() - start
        );
    }
}