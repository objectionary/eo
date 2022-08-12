/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
 * An attribute that logs all its operations to the console (very
 * convenient for debugging).
 *
 * @since 0.24
 */
final class AtLogged implements Attr {

    /**
     * Origin.
     */
    private final Attr origin;

    /**
     * Owner.
     */
    private final String owner;

    /**
     * Ctor.
     * @param attr Attribute
     * @param label Label
     */
    AtLogged(final Attr attr, final String label) {
        this.origin = attr;
        this.owner = label;
    }

    @Override
    public String toString() {
        return this.origin.toString();
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    @Override
    public Attr copy(final Phi self) {
        System.out.printf("  %s.copy()...\n", this.owner);
        final Attr ret = this.origin.copy(self);
        System.out.printf("  %s.copy()!\n", this.owner);
        return ret;
    }

    @Override
    public Phi get() {
        System.out.printf("  %s.get()...\n", this.owner);
        final Phi ret = this.origin.get();
        System.out.printf("  %s.get()! -> %d\n", this.owner, ret.hashCode());
        return ret;
    }

    @Override
    public void put(final Phi src) {
        System.out.printf("  %s.put()...\n", this.owner);
        this.origin.put(src);
        System.out.printf("  %s.put()!\n", this.owner);
    }

}
