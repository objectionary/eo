/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.BiFunc;
import org.cactoos.func.UncheckedBiFunc;

/**
 * Footprint that behaves like one of the given {@link Footprint}s depending on the give
 * condition.
 * @since 0.41
 */
final class FpFork implements Footprint {
    /**
     * Lazy condition.
     */
    private final UncheckedBiFunc<Path, Path, Boolean> condition;

    /**
     * First wrapped footprint.
     */
    private final Footprint first;

    /**
     * Second wrapped footprint.
     */
    private final Footprint second;

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
        this.condition = new UncheckedBiFunc<>(condition);
        this.first = first;
        this.second = second;
    }

    @Override
    public Path apply(final Path source, final Path target) throws IOException {
        final Footprint footprint;
        if (this.condition.apply(source, target)) {
            footprint = this.first;
        } else {
            footprint = this.second;
        }
        return footprint.apply(source, target);
    }
}
