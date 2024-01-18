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
package org.eolang.maven.name;

import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.hash.CommitHash;

/**
 * Swapped object name.
 * Depends on encapsulated condition behaves like one of the encapsulated object names.
 * If a second object is not provided - behaves like {@link OnUnversioned}
 *
 * @since 0.29.6
 */
public final class OnSwap implements ObjectName {
    /**
     * Swapped object name.
     */
    private final Unchecked<ObjectName> swapped;

    /**
     * Ctor.
     * @param condition Condition.
     * @param def Default object name.
     */
    public OnSwap(final boolean condition, final ObjectName def) {
        this(condition, def, new OnUnversioned(def));
    }

    /**
     * Ctor.
     * @param condition Condition.
     * @param first First object name.
     * @param second Second object name.
     */
    public OnSwap(
        final boolean condition,
        final ObjectName first,
        final ObjectName second
    ) {
        this.swapped = new Unchecked<>(
            new Sticky<>(
                () -> {
                    final ObjectName name;
                    if (condition) {
                        name = first;
                    } else {
                        name = second;
                    }
                    return name;
                }
            )
        );
    }

    @Override
    public String value() {
        return this.swapped.value().value();
    }

    @Override
    public CommitHash hash() {
        return this.swapped.value().hash();
    }

    @Override
    public String toString() {
        return String.valueOf(this.swapped.value());
    }
}
