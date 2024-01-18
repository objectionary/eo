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
import org.cactoos.Fallback;
import org.cactoos.Input;
import org.cactoos.func.FuncWithFallback;
import org.cactoos.func.IoCheckedFunc;

/**
 * Objectionary with fallback.
 *
 * @since 1.0
 */
public final class OyFallback implements Objectionary {

    /**
     * Primary Objectionary.
     */
    private final Objectionary first;

    /**
     * Fallback Objectionary.
     */
    private final Objectionary second;

    /**
     * Ctor.
     * @param primary Primary source.
     * @param secondary Secondary source.
     */
    public OyFallback(final Objectionary primary, final Objectionary secondary) {
        this.first = primary;
        this.second = secondary;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Input get(final String name) throws IOException {
        return new IoCheckedFunc<>(
            new FuncWithFallback<>(
                this.first::get,
                new Fallback.From<>(
                    IOException.class,
                    ex -> this.second.get(name)
                )
            )
        ).apply(name);
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(final String name) throws IOException {
        return this.first.contains(name) || this.second.contains(name);
    }

    @Override
    public String toString() {
        return String.format(
            "[%s]+[fallback to %s]",
            this.first, this.second
        );
    }
}
