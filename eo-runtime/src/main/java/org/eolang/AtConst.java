/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

/**
 * Const attribute.
 *
 * @since 0.16
 */
public final class AtConst implements Attr {

    private final Attr origin;

    private final AtomicReference<Phi> ref;

    public AtConst(final Attr attr) {
        this(attr, null);
    }

    public AtConst(final Attr attr, final Phi phi) {
        this.origin = attr;
        this.ref = new AtomicReference<>(phi);
    }

    @Override
    public String toString() {
        return String.format("%s!", this.origin.toString());
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtConst(this.origin.copy(self), this.ref.get());
    }

    @Override
    public Phi get() {
        if (this.ref.get() == null) {
            this.ref.set(this.origin.get());
        }
        return this.ref.get();
    }

    @Override
    public void put(final Phi src) {
        throw new Attr.ReadOnlyException(
            "An attribute of a const object can't be reset"
        );
    }

}
