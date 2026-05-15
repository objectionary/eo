/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
final class Compiling implements Step {

    /**
     * Assembling step.
     */
    private final Step assembling;

    /**
     * Linting step.
     */
    private final Step linting;

    /**
     * Resolving step.
     */
    private final Step resolving;

    /**
     * Placing step.
     */
    private final Step placing;

    /**
     * Constructor.
     * @param asmbl Assembling step
     * @param lnt Linting step
     * @param rslv Resolving step
     * @param plc Placing step
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    Compiling(
        final Step asmbl,
        final Step lnt,
        final Step rslv,
        final Step plc
    ) {
        this.assembling = asmbl;
        this.linting = lnt;
        this.resolving = rslv;
        this.placing = plc;
    }

    @Override
    public void exec() throws IOException {
        this.assembling.exec();
        this.linting.exec();
        this.resolving.exec();
        this.placing.exec();
    }
}
