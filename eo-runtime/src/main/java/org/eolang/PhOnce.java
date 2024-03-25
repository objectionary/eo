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

package org.eolang;

import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

/**
 * An object wrapping another one via caching lambda.
 *
 * @since 0.1
 */
@Versionized
class PhOnce implements Phi {
    /**
     * The object fetched.
     */
    private final Supplier<Phi> object;

    /**
     * Supplier to get term representation.
     */
    private final Supplier<String> term;

    /**
     * Supplier to get string representation.
     */
    private final Supplier<String> string;

    /**
     * Reference.
     */
    private final AtomicReference<Phi> reference;

    /**
     * Ctor.
     * @param obj Fetched object
     * @param str String representation
     * @param trm Term representation
     */
    PhOnce(final Supplier<Phi> obj, final Supplier<String> str, final Supplier<String> trm) {
        this.reference = new AtomicReference<>();
        this.object = () -> {
            synchronized (this.reference) {
                return this.reference.updateAndGet(
                    t -> {
                        final Phi result;
                        if (t == null) {
                            result = obj.get();
                        } else {
                            result = t;
                        }
                        return result;
                    }
                );
            }
        };
        this.string = str;
        this.term = trm;
    }

    @Override
    public boolean equals(final Object obj) {
        return this.object.get().equals(obj);
    }

    @Override
    public int hashCode() {
        return this.object.get().hashCode();
    }

    @Override
    public final String toString() {
        return this.string.get();
    }

    @Override
    public final String φTerm() {
        return this.term.get();
    }

    @Override
    public final Phi copy() {
        return this.object.get().copy();
    }

    @Override
    public final Attr attr(final int pos) {
        return this.object.get().attr(pos);
    }

    @Override
    public final Attr attr(final String name) {
        return this.object.get().attr(name);
    }

    @Override
    public String locator() {
        return this.object.get().locator();
    }

    @Override
    public String forma() {
        return this.object.get().forma();
    }

    @Override
    public byte[] data() {
        return this.object.get().data();
    }
}
