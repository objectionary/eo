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
package org.eolang.maven;

import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;
import org.eolang.maven.optimization.Optimization;
import org.eolang.maven.tojos.ForeignTojo;

/**
 * Optimized Tojos.
 * @since 0.34.0
 */
public class OptimizedTojos {

    /**
     * Collection of Foreign tojos of Mojo.
     */
    private final Iterable<ForeignTojo> tojos;

    /**
     * Abstraction for XML optimizations of Mojo.
     */
    private final Optimization optimization;

    /**
     * Converted tojo to optimization task.
     */
    private final OptimizationTask task;

    /**
     * Ctor.
     * @param tjs Collection of ForeignTojo.
     * @param opt Optimizations.
     * @param task Optimization task.
     */
    public OptimizedTojos(
        final Iterable<ForeignTojo> tjs,
        final Optimization opt,
        final OptimizationTask task
    ) {
        this.tojos = tjs;
        this.optimization = opt;
        this.task = task;
    }

    /**
     * Counting task.
     *
     * @return Integer count tasks.
     */
    public int count() {
        return new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors(),
                new Mapped<>(
                    tojo -> this.task.value(tojo, this.optimization),
                    this.tojos
                )
            )
        ).intValue();
    }
}
