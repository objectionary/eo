/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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

package org.eolang.phi;

/**
 * Vararg attribute.
 *
 * @since 0.1
 */
public final class AtVararg implements Attr {

    private final Attr origin;

    public AtVararg() {
        this(new AtSimple(AtVararg.empty()));
    }

    public AtVararg(final Attr attr) {
        this.origin = attr;
    }

    @Override
    public String toString() {
        return this.origin.toString();
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtVararg(this.origin.copy(self));
    }

    @Override
    public Phi get() {
        return this.origin.get();
    }

    @Override
    public void put(final Phi phi) {
        final Phi array = this.get();
        final Phi push = array.attr("push").get().copy();
        push.attr(0).put(phi);
        new Data.Take(push).take(Long.class);
    }

    private static Phi empty() {
        final Phi empty = new org.eolang.EOarray();
        empty.attr("data").put(new Data.Value<>(new Phi[] {}));
        return empty;
    }
}
