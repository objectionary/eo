/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
public interface Phi {

    /**
     * The global scope object, which owns all other objects.
     *
     * @checkstyle ConstantNameCheck (5 lines)
     */
    Phi Φ = new Phi() {
        @Override
        public String toString() {
            return "Φ";
        }

        @Override
        public Phi copy(final Phi rho) {
            return Phi.Φ;
        }

        @Override
        public Attr attr(final int pos) {
            return this.attr(String.format("#%d", pos));
        }

        @Override
        public Attr attr(final String name) {
            return new AtAbsent(name, " in Φ");
        }
    };

    /**
     * Make a copy, attaching it to a new parent.
     *
     * @param rho New \rho to attach to
     * @return A copy
     */
    Phi copy(Phi rho);

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
     * Compact toString.
     *
     * @since 0.17
     */
    final class Compact {
        /**
         * Max length.
         */
        private static final int MAX = 64;

        /**
         * Triple.
         */
        private static final int LEN = 3;

        /**
         * Original.
         */
        private final Phi phi;

        /**
         * Ctor.
         * @param obj The object
         */
        Compact(final Phi obj) {
            this.phi = obj;
        }

        @Override
        public String toString() {
            String txt = this.phi.toString()
                .replace('\n', ' ')
                .replaceAll(", +", ", ");
            if (txt.length() > Phi.Compact.MAX) {
                txt = String.format(
                    "%s...%s",
                    txt.substring(0, Phi.Compact.MAX - Phi.Compact.LEN),
                    txt.substring(Phi.Compact.MAX + Phi.Compact.LEN)
                );
            }
            return txt;
        }
    }

}
