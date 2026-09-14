/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import java.io.IOException;

/**
 * One fire of a λ function phino asked the engine to serve.
 *
 * <p>It answers the φ-expression phino goes on rewriting with, and
 * whatever it had to remember on the way is already a row of the symbol
 * table.</p>
 *
 * @since 0.76.0
 */
@FunctionalInterface
interface Fire {

    /**
     * The answer phino goes on with.
     *
     * @return The φ-expression
     * @throws IOException If the table or the wire fails
     * @throws InterruptedException If a wait for phino is interrupted
     */
    String answer() throws IOException, InterruptedException;
}
