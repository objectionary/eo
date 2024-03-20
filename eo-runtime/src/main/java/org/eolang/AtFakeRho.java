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
 * Fake \rho attribute.
 * 1. If retrieved \rho object is absent - the exception is thrown, see {@link AtRho}.
 * 2. If retrieved \rho object is equal to given "parent" object - it is replaced with given
 * "rho" object
 * 3. Otherwise, the retrieved \rho object is returned.
 * @since 0.36.0
 */
final class AtFakeRho implements Attr {
    /**
     * Original rho attribute.
     */
    private final Attr origin;

    /**
     * Possible current rho.
     */
    private final Phi current;

    /**
     * Possible alternate rho.
     */
    private final Phi alternate;

    /**
     * Ctor.
     * @param attr Original \rho attribute
     * @param parent Possible current \rho
     * @param rho Possible alternate \rho
     */
    public AtFakeRho(final Attr attr, final Phi parent, final Phi rho) {
        this.origin = attr;
        this.current = parent;
        this.alternate = rho;
    }

    @Override
    public Attr copy(final Phi self) {
        throw new IllegalStateException(
            "Should never happen"
        );
    }

    @Override
    public Phi get() {
        final Phi ret;
        final Phi rho = this.origin.get();
        if (rho.equals(this.current)) {
            ret = this.alternate;
        } else {
            ret = rho;
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
}
