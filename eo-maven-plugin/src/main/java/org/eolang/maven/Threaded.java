/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
final class Threaded<T> {

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
    Threaded(final Iterable<T> src, final Func<T, Integer> fun) {
        this.sources = src;
        this.scalar = fun;
    }

    /**
     * Exec them all and count.
     * @return How many succeeded
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    int total() {
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
                                    "Failed to process \"%s\" (%s)",
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
