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
 * Object with fake rho attribute.
 * @since 0.36.0
 */
public class PhFakeRho implements Phi {
    /**
     * Original phi.
     */
    private final Phi origin;

    /**
     * Rho object.
     */
    private final Phi rho;

    /**
     * Rho attribute.
     */
    private final AtomicReference<Attr> attribute;

    /**
     * Ctor.
     * @param phi Original object
     * @param rho Rho to overwrite
     */
    public PhFakeRho(final Phi phi, final Phi rho) {
        this.origin = phi;
        this.rho = rho;
        this.attribute = new AtomicReference<>(null);
    }

    @Override
    public Phi copy() {
        return new PhFakeRho(this.origin, this.rho);
    }

    @Override
    public Attr attr(final int pos) {
        return this.origin.attr(pos);
    }

    @Override
    public Attr attr(final String name) {
        final Attr attr;
        if (name.equals(Attr.RHO)) {
            synchronized (this.attribute) {
                if (this.attribute.get() == null) {
                    this.attribute.set(new AtSimple(this.rho));
                }
            }
            attr = this.attribute.get();
        } else {
            attr = this.origin.attr(name);
        }
        return attr;
    }

    @Override
    public String locator() {
        return this.origin.locator();
    }

    @Override
    public String forma() {
        return this.origin.forma();
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    @Override
    public String toString() {
        return this.origin.toString();
    }
}
