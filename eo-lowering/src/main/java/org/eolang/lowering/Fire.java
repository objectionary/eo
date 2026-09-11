/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;

/**
 * One fire of a λ function phino asked the engine to serve.
 *
 * @since 0.76.0
 */
public interface Fire {

    /**
     * The answer phino goes on with.
     *
     * @return The φ-expression
     * @throws IOException If the table or the wire fails
     * @throws InterruptedException If a wait for phino is interrupted
     */
    String answer() throws IOException, InterruptedException;
}
