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
 * Object with possible fake \rho attribute.
 * When \rho attribute is retrieved from the object, the attribute is
 * wrapped with {@link AtFakeRho}.
 * The word "possibly" is used because attribute may be absent, or retrieved \rho object may not
 * need to be reset.
 * @since 0.36.0
 */
final class PhFakeRho implements Phi {
    /**
     * Original object.
     */
    private final Phi origin;

    /**
     * Possible parent.
     */
    private final Phi parent;

    /**
     * Possible \rho.
     */
    private final Phi rho;

    /**
     * The cache for \rho attribute.
     */
    private final AtomicReference<Attr> attribute;

    /**
     * Ctor.
     * @param phi Original object
     * @param parent Possible original \rho
     * @param rho The possible \rho to replace with the original one
     */
    PhFakeRho(final Phi phi, final Phi parent, final Phi rho) {
        this.origin = phi;
        this.parent = parent;
        this.rho = rho;
        this.attribute = new AtomicReference<>(null);
    }

    @Override
    public Phi copy() {
        return new PhFakeRho(this.origin.copy(), this.parent, this.rho);
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
                    this.attribute.set(
                        new AtFakeRho(
                            this.origin.attr(name),
                            this.parent,
                            this.rho
                        )
                    );
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

    @Override
    public byte[] data() {
        return this.origin.data();
    }
}
