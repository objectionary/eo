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
final class AtConst implements Attr {

    private final Attr origin;

    private final Phi rho;

    private final AtomicReference<Phi> cache;

    AtConst(final Attr attr, final Phi phi) {
        this(attr, phi, null);
    }

    private AtConst(final Attr attr, final Phi phi, final Phi cached) {
        this.origin = attr;
        this.rho = phi;
        this.cache = new AtomicReference<>(cached);
    }

    @Override
    public String toString() {
        return String.format("%s!", this.origin.toString());
    }

    @Override
    public String φTerm() {
        return String.format("%s!", this.origin.φTerm());
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtConst(this.origin.copy(self), this.rho, this.cache.get());
    }

    @Override
    public Phi get() {
        synchronized (this.cache) {
            if (this.cache.get() == null) {
                final Phi phi = this.origin.copy(this.rho).get().copy();
                phi.move(this.rho);
                this.cache.set(phi);
            }
        }
        return this.cache.get();
    }

    @Override
    public void put(final Phi src) {
        this.origin.put(src);
    }

}
