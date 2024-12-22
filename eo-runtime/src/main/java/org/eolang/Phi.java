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
 * A simple object.
 *
 * <p>We call it Phi because of the name of the φ-calculus. Actually, a better
 * name would be "Object", but it's already occupied by Java. That's why
 * we call it Phi.</p>
 *
 * <p>It is guaranteed that the hash codes of different Phi are different,
 * and equal to the vertex.</p>
 *
 * @since 0.1
 */
public interface Phi extends Data {

    /**
     * The global scope object, which owns all other objects.
     *
     * @checkstyle ConstantNameCheck (5 lines)
     * @checkstyle AnonInnerLengthCheck (30 lines)
     */
    @SuppressWarnings("PMD.FieldNamingConventions")
    Phi Φ = new Phi() {
        /**
         * Default package.
         */
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
        public Phi copy() {
            return Phi.Φ;
        }

        @Override
        public Phi take(final String name) {
            return this.pkg.take(name);
        }

        @Override
        public boolean put(final int pos, final Phi object) {
            throw new IllegalStateException(
                String.format("Can't #put(%d, %s) to Φ", pos, object)
            );
        }

        @Override
        public boolean put(final String name, final Phi object) {
            throw new IllegalStateException(
                String.format("Can't #put(%s, %s) to Φ", name, object)
            );
        }

        @Override
        public String locator() {
            return "?:?";
        }

        @Override
        public String forma() {
            return this.pkg.forma();
        }

        @Override
        public byte[] delta() {
            throw new IllegalStateException(
                "Can't #data() from Ф"
            );
        }
    };

    /**
     * Make a copy, leaving it at the same parent.
     *
     * @return A copy
     */
    Phi copy();

    /**
     * Take object by name of the attribute.
     * @param name The name of the attribute
     * @return The object
     */
    Phi take(String name);

    /**
     * Put object by position of the attribute.
     * @param pos The position of the attribute.
     * @param object The object to put
     * @return Was attribute set
     */
    boolean put(int pos, Phi object);

    /**
     * Put object by name of the attribute.
     * @param name The name of the attribute.
     * @param object The object to put
     * @return Was attribute set
     */
    boolean put(String name, Phi object);

    /**
     * Get code locator of the phi.
     * @return String containing code locator
     */
    String locator();

    /**
     * Get forma of the phi.
     * @return Forma of it as {@link String}.
     */
    String forma();
}
