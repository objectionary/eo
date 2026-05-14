/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;

/**
 * Core compilation orchestration: runs Assembling, Linting, Resolving, and Placing in sequence.
 *
 * <p>
 *     This class combines {@link Assembling}, {@link Linting}, {@link Resolving} and
 *     {@link Placing} steps into a single sequential execution.
 *     See their documentation for more details.
 * </p>
 *
 * @since 0.61.0
 */
final class Compiling {

    /**
     * Assembling step.
     */
    private final Assembling assembling;

    /**
     * Linting step.
     */
    private final Linting linting;

    /**
     * Resolving step.
     */
    private final Resolving resolving;

    /**
     * Placing step.
     */
    private final Placing placing;

    /**
     * Constructor.
     * @param asmbl Assembling step
     * @param lnt Linting step
     * @param rslv Resolving step
     * @param plc Placing step
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    Compiling(
        final Assembling asmbl,
        final Linting lnt,
        final Resolving rslv,
        final Placing plc
    ) {
        this.assembling = asmbl;
        this.linting = lnt;
        this.resolving = rslv;
        this.placing = plc;
    }

    /**
     * Execute the full compilation pipeline.
     * @throws IOException If any step fails
     */
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void exec() throws IOException {
        final long begin = System.currentTimeMillis();
        this.assembling.exec();
        this.linting.exec();
        this.resolving.exec();
        this.placing.exec();
        Logger.info(
            this,
            "Compilation process took %[ms]s",
            System.currentTimeMillis() - begin
        );
    }
}
