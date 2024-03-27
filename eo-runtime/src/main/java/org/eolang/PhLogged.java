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
    public Phi take(final int pos) {
        System.out.printf("%d.take(\"%d\")...\n", this.hashCode(), pos);
        final Phi ret = this.origin.take(pos);
        System.out.printf("%d.take(\"%d\")!\n", this.hashCode(), pos);
        return ret;
    }

    @Override
    public Phi take(final String name) {
        System.out.printf("%d.take(\"%s\")...\n", this.hashCode(), name);
        final Phi ret = this.origin.take(name);
        System.out.printf("%d.take(\"%s\")!\n", this.hashCode(), name);
        return ret;
    }

    @Override
    public Phi take(final String name, final Phi rho) {
        System.out.printf("%d.take(\"%s\", %d)...\n", this.hashCode(), name, rho.hashCode());
        final Phi ret = this.origin.take(name);
        System.out.printf("%d.take(\"%s\", %d)!\n", this.hashCode(), name, rho.hashCode());
        return ret;
    }

    @Override
    public void put(final int pos, final Phi object) {
        System.out.printf("%d.put(%d, %d)...\n", this.hashCode(), pos, object.hashCode());
        this.origin.put(pos, object);
        System.out.printf("%d.put(%d, %d)!\n", this.hashCode(), pos, object.hashCode());
    }

    @Override
    public void put(final String name, final Phi object) {
        System.out.printf("%d.put(\"%s\", %d)...\n", this.hashCode(), name, object.hashCode());
        this.origin.put(name, object);
        System.out.printf("%d.put(\"%s\", %d)!\n", this.hashCode(), name, object.hashCode());
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
