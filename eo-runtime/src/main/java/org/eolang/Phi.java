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
 * A simple object.
 *
 * We call it Phi because of the name of the φ-calculus. Actually, a better
 * name would be "Object", but it's already occupied by Java. That's why
 * we call it Phi.
 *
 * @since 0.1
 */
public interface Phi extends Term {

    /**
     * The global scope object, which owns all other objects.
     *
     * @checkstyle ConstantNameCheck (5 lines)
     * @checkstyle AnonInnerLengthCheck (30 lines)
     */
    Phi Φ = new Phi() {
        private final Phi pkg = new PhPackage("");

        @Override
        public boolean equals(final Object obj) {
            return obj == this;
        }

        @Override
        public int hashCode() {
            return 0;
        }

        @Override
        public String φTerm() {
            return "Φ";
        }

        @Override
        public String toString() {
            return "Φ";
        }

        @Override
        public Phi copy() {
            return Phi.Φ;
        }

        @Override
        public void move(final Phi rho) {
            throw new ExFailure("Can't #move() root object");
        }

        @Override
        public Attr attr(final int pos) {
            throw new ExFailure(
                String.format("Can't #attr(%d) in Φ", pos)
            );
        }

        @Override
        public Attr attr(final String name) {
            return this.pkg.attr(name);
        }

        @Override
        public String location() {
            return "?:?";
        }
    };

    /**
     * Make a copy, leaving it at the same parent.
     *
     * @return A copy
     */
    Phi copy();

    /**
     * Move it to a new parent.
     *
     * @param rho New \rho to attach to
     */
    void move(Phi rho);

    /**
     * Get attribute by position.
     *
     * @param pos The position of the attribute
     * @return The attr
     */
    Attr attr(int pos);

    /**
     * Get attribute.
     *
     * @param name The name of the attribute
     * @return The attr
     */
    Attr attr(String name);

    /**
     * Get code location of the phi.
     * @return String containing code location
     */
    String location();
}
