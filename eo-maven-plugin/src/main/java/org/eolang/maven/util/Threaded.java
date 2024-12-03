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
package org.eolang.maven.util;

import org.cactoos.Func;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Mapped;
import org.cactoos.number.SumOf;

/**
 * Processes elements in multiple threads.
 *
 * <p>We use this class to process a large number of elements in parallel,
 * using all available processors. It is used in most Mojos, where multiple
 * files must be processed parallel.</p>
 *
 * @param <T> The type of the element to iterate
 * @since 0.1
 */
public final class Threaded<T> {

    /**
     * The sources.
     */
    private final Iterable<T> sources;

    /**
     * The function.
     */
    private final Func<T, Integer> scalar;

    /**
     * Ctor.
     * @param src The sources
     * @param fun The function to run
     */
    public Threaded(final Iterable<T> src, final Func<T, Integer> fun) {
        this.sources = src;
        this.scalar = fun;
    }

    /**
     * Exec them all and count.
     * @return How many succeeded
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    public int total() {
        return new SumOf(
            new Threads<>(
                Runtime.getRuntime().availableProcessors() * 2,
                new Mapped<>(
                    tojo -> () -> {
                        try {
                            return this.scalar.apply(tojo);
                            // @checkstyle IllegalCatchCheck (1 line)
                        } catch (final Exception ex) {
                            throw new IllegalStateException(
                                String.format(
                                    "Failed to process %s (%s)",
                                    tojo, tojo.getClass().getCanonicalName()
                                ),
                                ex
                            );
                        }
                    },
                    this.sources
                )
            )
        ).intValue();
    }

}
