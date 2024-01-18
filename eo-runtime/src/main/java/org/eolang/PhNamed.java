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
 * A named object.
 *
 * @since 0.17
 */
@Versionized
final class PhNamed implements Phi {

    /**
     * The original.
     */
    private final Phi origin;

    /**
     * The name.
     */
    private final String name;

    /**
     * Ctor.
     *
     * @param phi The object
     * @param txt The name
     */
    PhNamed(final Phi phi, final String txt) {
        this.origin = phi;
        this.name = txt;
    }

    @Override
    public boolean equals(final Object obj) {
        return this.origin.equals(obj);
    }

    @Override
    public int hashCode() {
        return this.origin.hashCode();
    }

    @Override
    public String toString() {
        return String.format("%s≡%s", this.name, this.origin.toString());
    }

    @Override
    public String φTerm() {
        return String.format("%s ≡ %s", this.name, this.origin.φTerm());
    }

    @Override
    public Phi copy() {
        return new PhNamed(this.origin.copy(), this.name);
    }

    @Override
    public Attr attr(final int pos) {
        return new AtNamed(this.name, this.name, this, this.origin.attr(pos));
    }

    @Override
    public Attr attr(final String attr) {
        return new AtNamed(this.name, this.name, this, this.origin.attr(attr));
    }

    @Override
    public String locator() {
        return this.origin.locator();
    }

    @Override
    public String forma() {
        return this.origin.forma();
    }

}
