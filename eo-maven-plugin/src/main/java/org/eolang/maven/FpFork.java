/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Path;
import org.cactoos.BiFunc;
import org.cactoos.func.UncheckedBiFunc;

/**
 * Footprint that behaves like one of the given {@link Footprint}s depending on the give
 * condition.
 * @since 0.41
 */
final class FpFork extends FpEnvelope {
    /**
     * Ctor.
     * @param condition Condition as boolean
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    FpFork(final boolean condition, final Footprint first, final Footprint second) {
        this((src, tgt) -> condition, first, second);
    }

    /**
     * Ctor.
     * @param condition Lazy condition
     * @param first First wrapped footprint
     * @param second Second wrapped footprint
     */
    FpFork(
        final BiFunc<Path, Path, Boolean> condition, final Footprint first, final Footprint second
    ) {
        super(
            (src, tgt) -> {
                final Footprint footprint;
                if (new UncheckedBiFunc<>(condition).apply(src, tgt)) {
                    footprint = first;
                } else {
                    footprint = second;
                }
                return footprint.apply(src, tgt);
            }
        );
    }
}
