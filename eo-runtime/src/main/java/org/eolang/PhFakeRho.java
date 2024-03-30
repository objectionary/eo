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

/**
 * Object which \rho is faked.
 * @since 0.36.0
 */
class PhFakeRho implements Phi {
    /**
     * Original object.
     */
    private final Phi origin;

    /**
     * Possible parent of object.
     */
    private final Phi parent;

    /**
     * Rho that original one should be replaced with.
     */
    private final Phi rho;

    /**
     * Ctor.
     * @param phi Original object
     * @param parent Possible parent
     * @param rho Rho that original one should be replaced with
     */
    PhFakeRho(final Phi phi, final Phi parent, final Phi rho) {
        this.origin = phi;
        this.parent = parent;
        this.rho = rho;
    }

    @Override
    public byte[] delta() {
        return this.origin.delta();
    }

    @Override
    public Phi copy() {
        return new PhFakeRho(this.origin.copy(), this.parent, this.rho);
    }

    @Override
    public Phi take(final String name) {
        return this.take(name, this);
    }

    @Override
    public Phi take(final String name, final Phi rho) {
        Phi object = this.origin.take(name, rho);
        if (name.equals(Attr.RHO) && this.parent.equals(object)) {
            object = this.rho;
        }
        return object;
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.origin.put(pos, object);
    }

    @Override
    public void put(final String name, final Phi object) {
        this.origin.put(name, object);
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
}
