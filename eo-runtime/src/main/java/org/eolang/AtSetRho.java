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
 * Attribute that tries to set \rho to retrieved object.
 * Attribute does not set \rho if retrieved object is \rho or \sigma.
 * Since every \rho attribute of {@link Phi} is {@link AtRho} it won't be
 * reset because {@link AtRho} ignores all puts except first.
 * @since 0.36.0
 */
public final class AtSetRho implements Attr {
    /**
     * Original attribute.
     */
    private final Attr origin;

    /**
     * Rho to put.
     */
    private final Phi rho;

    /**
     * Name of the attribute to get.
     */
    private final String name;

    /**
     * Ctor.
     * @param attr Origin attribute
     * @param rho Rho that will be set
     * @param name Name of the attribute
     */
    public AtSetRho(final Attr attr, final Phi rho, final String name) {
        this.origin = attr;
        this.rho = rho;
        this.name = name;
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtSetRho(this.origin.copy(self), self, this.name);
    }

    @Override
    public Phi get() {
        final Phi ret = this.origin.get();
        if (!this.name.equals(Attr.RHO) && !this.name.equals(Attr.SIGMA)) {
            ret.attr(Attr.RHO).put(this.rho);
        }
        return ret;
    }

    @Override
    public void put(final Phi phi) {
        this.origin.put(phi);
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
