/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;

/**
 * A single pipeline step.
 * @since 0.68.0
 */
@SuppressWarnings("PMD.ImplicitFunctionalInterface")
interface Step {

    /**
     * Execute this step.
     * @throws IOException If fails
     */
    void exec() throws IOException;
}