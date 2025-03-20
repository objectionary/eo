/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import org.cactoos.Func;

/**
 * Footprint that behaves as first given wrapped {@link Footprint}
 * if provided target exists and actual toward source.
 * Behaves as second given wrapped {@link Footprint} otherwise.
 * @since 0.41
 */
final class FpIfActual extends FpEnvelope {
    /**
     * Ctor.
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    FpIfActual(final Footprint first, final Footprint second) {
        this(target -> target, first, second);
    }

    /**
     * Ctor.
     * @param destination Function that modifies result target path
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    FpIfActual(
        final Func<Path, Path> destination, final Footprint first, final Footprint second
    ) {
        super(
            new FpIfTargetExists(
                destination,
                new FpIfTargetActual(
                    destination,
                    first,
                    second
                ),
                second
            )
        );
    }
}
