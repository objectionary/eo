/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

/**
 * The {@code eo.step} indentation width, guaranteed readable by the parser.
 * @since 0.57.0
 */
final class ReadableStep {

    /**
     * The only indentation width the EO parser can read back.
     */
    private static final int READABLE = 2;

    /**
     * The configured step.
     */
    private final int step;

    /**
     * Ctor.
     * @param configured The configured step
     */
    ReadableStep(final int configured) {
        this.step = configured;
    }

    /**
     * The step, rejected here (named, with a clear message) instead of
     * crashing or corrupting output further down the pipeline.
     * @return The step, guaranteed to be readable back
     */
    int value() {
        if (this.step != ReadableStep.READABLE) {
            throw new IllegalArgumentException(
                String.format(
                    "The 'eo.step' parameter must be %d, since that is the only indentation width the EO parser can read back; got %d",
                    ReadableStep.READABLE, this.step
                )
            );
        }
        return this.step;
    }
}
