/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;

/**
 * One stage of the lowering pipeline.
 *
 * <p>A stage reads what the stages before it left in the lowering
 * directory, writes its own product there, and fails on anything it
 * cannot do, since lowering never skips and never retries. A formation
 * the pipeline gives up on is a formation left as it was written, while a
 * pipeline that carried on past a broken file would fold half a program
 * and say nothing about the other half.</p>
 *
 * @since 0.74.0
 */
@FunctionalInterface
public interface Stage {

    /**
     * Do the work of this stage.
     *
     * @throws IOException If anything the stage needs cannot be read or
     *  written
     */
    void exec() throws IOException;
}
