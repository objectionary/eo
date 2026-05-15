/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;

/**
 * A single pipeline step that can be executed.
 * @since 0.61.0
 */
interface Step {

    /**
     * Execute the step.
     * @throws IOException If fails
     */
    void exec() throws IOException;
}
