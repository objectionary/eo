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

/**
 * Special attribute for \rho.
 *
 * <p>The attribute can be set only once, and it ignores all other puts.</p>
 *
 * @since 0.36.0
 */
final class AtRho implements Attr {
    /**
     * Rho.
     */
    private final AtomicReference<Phi> rho;

    /**
     * Ctor.
     */
    AtRho() {
        this(null);
    }

    /**
     * Ctor.
     * @param rho Rho.
     */
    private AtRho(final Phi rho) {
        this.rho = new AtomicReference<>(rho);
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtRho(this.rho.get());
    }

    @Override
    public Phi get() {
        if (this.rho.get() == null) {
            throw new ExUnset(
                String.format("The \"%s\" attribute is not set", Attr.RHO)
            );
        }
        return this.rho.get();
    }

    @Override
    public boolean put(final Phi phi) {
        final boolean ret;
        if (this.rho.get() == null) {
            this.rho.set(phi);
            ret = true;
        } else {
            ret = false;
        }
        return ret;
    }

    @Override
    public String φTerm() {
        final String term;
        if (this.rho.get() == null) {
            term = Term.EMPTY;
        } else {
            term = this.rho.get().φTerm();
        }
        return term;
    }
}
