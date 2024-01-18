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
 * Attribute for inner atoms.
 * @since 0.33.0
 * @checkstyle DesignForExtensionCheck (100 lines)
 */
public abstract class AtAtom implements Attr {
    /**
     * Original attribute.
     */
    private final Attr origin;

    /**
     * Ctor.
     * @param phi Phi to wrap.
     */
    public AtAtom(final Phi phi) {
        this(new AtSimple(phi));
    }

    /**
     * Ctor.
     * @param attr Attribute to wrap.
     */
    AtAtom(final Attr attr) {
        this.origin = new AtOnce(attr);
    }

    @Override
    public Attr copy(final Phi self) {
        return this.origin.copy(self);
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
    public Phi get() {
        return this.origin.get();
    }

    @Override
    public void put(final Phi phi) {
        this.origin.put(phi);
    }
}
