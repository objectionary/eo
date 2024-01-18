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
package org.eolang.maven.objectionary;

import java.io.IOException;
import org.cactoos.Input;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.PullMojo;

/**
 * Fallback which can swap primary/secondary repos.
 * <p/>
 * The key purpose of this class is to allow dynamic determination
 * of which Oy (fist or second) to use as primary and which as fallback based on given
 * boolean property.
 * <p/>
 * For {@link PullMojo} this is used to bypass reading from cache by always checking remote
 * first and only fallback to local in case of object miss:
 * <pre>
 *     new PullMojo.FallbackSwapOy(
 *         &lt local &gt,
 *         &lt remote &gt,
 *         this.forceUpdate()
 *     );
 * </pre>
 * @since 1.0
 */
public final class OyFallbackSwap implements Objectionary {
    /**
     * Swapped Oy.
     */
    private final Unchecked<Objectionary> swapped;

    /**
     * Ctor.
     * @param first Initial primary
     * @param second Initial secondary
     * @param swap Whether to swap
     */
    public OyFallbackSwap(
        final Objectionary first,
        final Objectionary second,
        final boolean swap
    ) {
        this(first, second, () -> swap);
    }

    /**
     * Ctor.
     * @param first Initial primary
     * @param second Initial secondary
     * @param swap Whether to swap
     */
    public OyFallbackSwap(
        final Objectionary first,
        final Objectionary second,
        final Scalar<Boolean> swap
    ) {
        this.swapped = new Unchecked<>(
            new Sticky<>(
                () -> {
                    final Objectionary result;
                    if (swap.value()) {
                        result = new OyFallback(
                            second,
                            first
                        );
                    } else {
                        result = new OyFallback(
                            first,
                            second
                        );
                    }
                    return result;
                }
            )
        );
    }

    @Override
    public Input get(final String name) throws IOException {
        return this.swapped.value().get(name);
    }

    @Override
    public boolean contains(final String name) throws IOException {
        return this.swapped.value().contains(name);
    }

    @Override
    public String toString() {
        return this.swapped.value().toString();
    }
}
