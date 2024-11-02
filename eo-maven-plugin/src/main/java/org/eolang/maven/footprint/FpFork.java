/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven.footprint;

import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.BiFunc;
import org.cactoos.func.UncheckedBiFunc;

/**
 * Footprint that behaves like one of the given {@link Footprint}s depending on the give
 * condition.
 * @since 0.41
 */
public final class FpFork implements Footprint {
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
     * @param condition Lazy condition
     * @param first First wrapped condition
     * @param second Second wrapped condition
     */
    public FpFork(
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
