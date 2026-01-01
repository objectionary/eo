/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.Func;
import org.cactoos.Input;
import org.cactoos.func.UncheckedFunc;

/**
 * Footprint that saves content generated from lambda to the target file.
 * @since 0.41
 */
final class FpGenerated implements Footprint {
    /**
     * Content function.
     */
    private final UncheckedFunc<Path, Input> content;

    /**
     * Ctor.
     * @param content Content as bytes
     */
    FpGenerated(final Func<Path, Input> content) {
        this.content = new UncheckedFunc<>(content);
    }

    @Override
    public Path apply(final Path source, final Path target) throws IOException {
        return new Saved(this.content.apply(source), target).value();
    }
}
