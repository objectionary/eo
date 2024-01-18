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
 * An object that reports all manipulations with it to the log (very useful
 * for debugging purposes).
 *
 * <p>This class is thread-safe.</p>
 *
 * @since 0.24
 */
@Versionized
public final class PhLogged implements Phi {

    /**
     * The origin being turned into a const.
     */
    private final Phi origin;

    /**
     * Ctor.
     *
     * @param phi The origin
     */
    public PhLogged(final Phi phi) {
        this.origin = phi;
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    @Override
    public Phi copy() {
        System.out.printf("%d.copy()...\n", this.hashCode());
        final Phi ret = this.origin.copy();
        System.out.printf("%d.copy()! -> %d\n", this.hashCode(), ret.hashCode());
        return ret;
    }

    @Override
    public Attr attr(final int pos) {
        System.out.printf("%d.attr(#%d)...\n", this.hashCode(), pos);
        final Attr ret = new AtLogged(
            this.origin.attr(pos),
            String.format("%d#%d", this.hashCode(), pos)
        );
        System.out.printf("%d.attr(#%d)!\n", this.hashCode(), pos);
        return ret;
    }

    @Override
    public Attr attr(final String name) {
        System.out.printf("%d.attr(\"%s\")...\n", this.hashCode(), name);
        final Attr ret = new AtLogged(
            this.origin.attr(name),
            String.format("%d#%s", this.hashCode(), name)
        );
        System.out.printf("%d.attr(\"%s\")!\n", this.hashCode(), name);
        return ret;
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
    public boolean equals(final Object obj) {
        return this.origin.equals(obj);
    }

    @Override
    public int hashCode() {
        return this.origin.hashCode();
    }

    @Override
    public String toString() {
        return this.origin.toString();
    }

}
