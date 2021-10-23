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

/**
 * Attribute with \rho set on fly.
 *
 * @since 0.16
 */
final class AtOwned implements Attr {

    /**
     * The original attribute.
     */
    private final Attr origin;

    /**
     * The parent to put into \rho attribute of the original object.
     */
    private final Phi parent;

    /**
     * Ctor.
     * @param attr The origin
     * @param prnt The value of \rho to use
     */
    AtOwned(final Attr attr, final Phi prnt) {
        this.origin = attr;
        this.parent = prnt;
    }

    @Override
    public String toString() {
        return this.origin.toString();
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtOwned(this.origin.copy(self), self);
    }

    @Override
    public Phi get() {
        final Phi phi = this.origin.get();
        if (!(phi instanceof Data)) {
            phi.attr("ρ").put(this.parent);
        }
        return phi;
    }

    @Override
    public void put(final Phi phi) {
        this.origin.put(phi);
    }
}
