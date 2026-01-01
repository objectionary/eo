/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;

/**
 * Wrapper for footprint.
 * @since 0.41
 * @checkstyle DesignForExtensionCheck (50 lines)
 */
class FpEnvelope implements Footprint {
    /**
     * Wrapped footprint.
     */
    private final Footprint origin;

    /**
     * Ctor.
     * @param footprint Wrapped footprint
     */
    FpEnvelope(final Footprint footprint) {
        this.origin = footprint;
    }

    @Override
    public Path apply(final Path source, final Path target) throws IOException {
        return this.origin.apply(source, target);
    }
}
