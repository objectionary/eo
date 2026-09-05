/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;

/**
 * Core compilation orchestration: runs Assembling, Linting, Merging, Resolving
 * and Placing in sequence.
 *
 * <p>This class combines {@link Assembling}, {@link Linting}, {@link Merging},
 * {@link Resolving} and {@link Placing} steps into a single sequential execution.
 * See their documentation for more details.</p>
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
     * Merging step.
     */
    private final Step merging;

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
     * @param mrg Merging step
     * @param rslv Resolving step
     * @param plc Placing step
     */
    Compiling(
        final Step asmbl,
        final Step lnt,
        final Step mrg,
        final Step rslv,
        final Step plc
    ) {
        this.assembling = asmbl;
        this.linting = lnt;
        this.merging = mrg;
        this.resolving = rslv;
        this.placing = plc;
    }

    @Override
    public void exec() throws IOException {
        this.assembling.exec();
        this.linting.exec();
        this.merging.exec();
        this.resolving.exec();
        this.placing.exec();
    }
}
