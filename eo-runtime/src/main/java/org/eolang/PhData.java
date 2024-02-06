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
 * Phi with encapsulated data as bytes.
 * @since 0.36.0
 */
public class PhData implements Phi {
    /**
     * Original phi.
     */
    private final Phi origin;

    /**
     * Data.
     */
    private final byte[] data;

    /**
     * Ctor.
     * @param phi Original phi
     * @param bytes Bytes
     */
    public PhData(final Phi phi, final byte[] bytes) {
        this.origin = phi;
        this.data = bytes;
    }

    @Override
    public byte[] take() {
        return this.data;
    }

    @Override
    public Phi copy() {
        return new PhData(this.origin.copy(), this.data);
    }

    @Override
    public Attr attr(final int pos) {
        return new AtOnce(new AtComposite(this, rho -> {
            final Phi phi = this.origin.attr(pos).get();
            phi.attr(Attr.RHO).put(rho);
            return phi;
        }));
    }

    @Override
    public Attr attr(final String name) {
        return new AtOnce(new AtComposite(this, rho -> {
            final Phi phi = this.origin.attr(name).get();
            phi.attr(Attr.RHO).put(rho);
            return phi;
        }));
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
    public String toString() {
        final StringBuilder out = new StringBuilder(0);
        out.append(this.origin.toString()).append("=");
        for (final byte bte : this.data) {
            if (out.length() > 0) {
                out.append('-');
            }
            out.append(String.format("%02X", bte));
        }
        if (out.length() == 0) {
            out.append('-');
        }
        return out.toString();
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }
}
