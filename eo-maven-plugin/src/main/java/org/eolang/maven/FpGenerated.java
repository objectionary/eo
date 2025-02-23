/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.Func;
import org.cactoos.scalar.ScalarOf;

/**
 * Footprint that saves content generated from lambda to the target file.
 * @since 0.41
 */
final class FpGenerated implements Footprint {
    /**
     * Content function.
     */
    private final Func<Path, String> content;

    /**
     * Ctor.
     * @param content Content function
     */
    FpGenerated(final Func<Path, String> content) {
        this.content = content;
    }

    @Override
    public Path apply(final Path source, final Path target) throws IOException {
        return new Saved(new ScalarOf<>(this.content, source), target).value();
    }
}
