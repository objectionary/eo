/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;

/**
 * The paths the transpiler writes into: where the parsed XMIR goes, where
 * the generated Java lands, the base cache directory, and the file where
 * XSL measurements are stored. They travel together, so they are passed
 * as one value instead of four {@code Path} parameters.
 * @since 0.62.0
 */
@SuppressWarnings("PMD.DataClass")
final class Outputs {

    /**
     * Target directory of the build.
     */
    private final Path target;

    /**
     * Generated sources directory.
     */
    private final Path generated;

    /**
     * Base cache directory.
     */
    private final Path cache;

    /**
     * File where XSL measurements are stored.
     */
    private final Path measures;

    /**
     * Ctor.
     * @param target Target directory of the build
     * @param generated Generated sources directory
     * @param cache Base cache directory
     * @param measures File where XSL measurements are stored
     */
    Outputs(
        final Path target, final Path generated, final Path cache, final Path measures
    ) {
        this.target = target;
        this.generated = generated;
        this.cache = cache;
        this.measures = measures;
    }

    /**
     * Target directory of the build.
     * @return The directory
     */
    Path target() {
        return this.target;
    }

    /**
     * Generated sources directory.
     * @return The directory
     */
    Path generated() {
        return this.generated;
    }

    /**
     * Base cache directory.
     * @return The directory
     */
    Path cache() {
        return this.cache;
    }

    /**
     * File where XSL measurements are stored.
     * @return The file
     */
    Path measures() {
        return this.measures;
    }
}
