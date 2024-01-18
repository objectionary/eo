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

import java.util.function.Supplier;

/**
 * An object wrapping another one.
 *
 * @since 0.1
 */
@Versionized
class PhOnce implements Phi {

    /**
     * The object fetched.
     */
    private final Data<Phi> object;

    /**
     * The expression provider.
     */
    private final Supplier<String> exp;

    /**
     * Ctor.
     *
     * @param data The object
     * @param blank The string value
     * @param expr The expression
     */
    PhOnce(final Data<Phi> data, final Supplier<String> blank,
        final Supplier<String> expr) {
        this.object = new Data.Once<>(data, blank);
        this.exp = expr;
    }

    @Override
    public boolean equals(final Object obj) {
        return this.object.take().equals(obj);
    }

    @Override
    public int hashCode() {
        return this.object.hashCode();
    }

    @Override
    public final String toString() {
        return this.object.toString();
    }

    @Override
    public final String φTerm() {
        return this.exp.get();
    }

    @Override
    public final Phi copy() {
        return this.object.take().copy();
    }

    @Override
    public final Attr attr(final int pos) {
        return this.object.take().attr(pos);
    }

    @Override
    public final Attr attr(final String name) {
        return this.object.take().attr(name);
    }

    @Override
    public String locator() {
        return this.object.take().locator();
    }

    @Override
    public String forma() {
        return this.object.take().forma();
    }
}
