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
 * \Rho attribute.
 * The attribute can be set only once, and it ignores all other puts.
 * When attribute is copied, the \rho inside isn't copied.
 * It allows to have the same \rho if objects were copied recursively:
 * <p>
 * {@code
 * [] > x
 *   [] > y
 * x.y.^ # absent
 * x' > x1
 * x1.y.^ # refers to x1
 * x1' > x2
 * x2.y.^ # refers to x1
 * }
 * </p>
 * @since 0.36.0
 */
public class AtRho implements Attr {
    /**
     * Rho.
     */
    private final AtomicReference<Phi> rho;

    /**
     * Ctor.
     */
    public AtRho() {
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
            throw new ExUnset("");
        }
        return this.rho.get();
    }

    @Override
    public void put(final Phi phi) {
        if (this.rho.get() == null) {
            this.rho.set(phi);
        }
    }

    @Override
    public String φTerm() {
        return null;
    }
}
