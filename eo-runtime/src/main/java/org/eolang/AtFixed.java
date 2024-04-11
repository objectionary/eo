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
 * Attribute that ignores putting and copying.
 * The only usage for the attribute is keeping \sigma
 * attribute.
 *
 * @since 0.1
 */
@Versionized
final class AtFixed implements Attr {

    /**
     * Phi that is fixed.
     */
    private final Phi fixed;

    /**
     * Attribute.
     */
    private final Attr attr;

    /**
     * Ctor.
     * @param phi Fixed phi
     */
    AtFixed(final Phi phi) {
        this.fixed = phi;
        this.attr = new AtSimple(this.fixed);
    }

    @Override
    public String toString() {
        return this.attr.toString();
    }

    @Override
    public String φTerm() {
        return this.attr.φTerm();
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtFixed(this.fixed);
    }

    @Override
    public Phi get() {
        return this.fixed;
    }

    @Override
    public boolean put(final Phi src) {
        return false;
    }
}
