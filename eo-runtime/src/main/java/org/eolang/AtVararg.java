/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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

import EOorg.EOeolang.EOtuple;
import EOorg.EOeolang.EOtuple$EOempty;

/**
 * Vararg attribute.
 *
 * @since 0.1
 */
@Versionized
public final class AtVararg implements Attr {
    /**
     * Result tuple.
     */
    private Phi tuple;

    /**
     * Ctor.
     */
    public AtVararg() {
        this.tuple = new EOtuple$EOempty(Phi.Φ);
    }

    @Override
    public String toString() {
        return String.format("%sV", this.tuple.toString());
    }

    @Override
    public String φTerm() {
        return this.tuple.φTerm();
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtVararg();
    }

    @Override
    public Phi get() {
        return this.tuple;
    }

    @Override
    public void put(final Phi phi) {
        if (phi instanceof PhUnvar) {
            this.tuple = phi;
        } else {
            final Phi outer = new EOtuple(Phi.Φ);
            outer.attr(0).put(this.tuple);
            outer.attr(1).put(phi);
            this.tuple = outer;
        }
    }
}
