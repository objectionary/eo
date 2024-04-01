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
 * Object with data.
 * @since 0.36.0
 */
public final class PhData implements Phi {

    /**
     * The original object.
     */
    private final Phi object;

    /**
     * Data.
     */
    private final byte[] data;

    /**
     * Ctor.
     * @param obj Original object
     * @param data Data
     */
    public PhData(final Phi obj, final byte[] data) {
        this.object = obj;
        this.data = data;
    }

    @Override
    public Phi copy() {
        return new PhData(this.object.copy(), this.data);
    }

    @Override
    public Phi take(final String name) {
        return this.take(name, this);
    }

    @Override
    public Phi take(final String name, final Phi rho) {
        return this.object.take(name, rho);
    }

    @Override
    public void put(final int pos, final Phi obj) {
        this.object.put(pos, obj);
    }

    @Override
    public void put(final String name, final Phi obj) {
        this.object.put(name, obj);
    }

    @Override
    public String locator() {
        return this.object.locator();
    }

    @Override
    public String forma() {
        return this.object.forma();
    }

    @Override
    public String φTerm() {
        return this.object.φTerm()
            .replaceFirst(
                "⟦\n",
                String.format(
                    "⟦\n\t%s ↦ %s,\n",
                    Attr.DELTA,
                    this.bytes()
                )
            );
    }

    @Override
    public byte[] delta() {
        return this.data;
    }

    @Override
    public String toString() {
        return String.format(
            "%s=%s",
            this.object.toString(),
            this.bytes()
        );
    }

    /**
     * Converts byte array to string.
     * @return Byte array as string
     */
    public String bytes() {
        final StringBuilder out = new StringBuilder(0);
        for (final byte bte : this.data) {
            if (out.length() > 0) {
                out.append('-');
            }
            out.append(String.format("%02X", bte));
        }
        if (this.data.length == 0) {
            out.append("--");
        }
        return out.toString();
    }
}
