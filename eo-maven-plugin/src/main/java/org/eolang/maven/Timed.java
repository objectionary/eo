/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;

/**
 * A {@link Step} decorator that measures and logs elapsed execution time.
 * @since 0.61.0
 */
final class Timed implements Step {

    /**
     * The wrapped step.
     */
    private final Step origin;

    /**
     * Constructor.
     * @param step Step to wrap
     */
    Timed(final Step step) {
        this.origin = step;
    }

    @Override
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    public void exec() throws IOException {
        final long start = System.currentTimeMillis();
        this.origin.exec();
        Logger.info(
            this,
            "%s took %[ms]s",
            this.origin.getClass().getSimpleName(),
            System.currentTimeMillis() - start
        );
    }
}
