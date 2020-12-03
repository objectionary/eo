/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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
 * Data primitive.
 *
 * @since 0.1
 */
public interface Data<T> {

    /**
     * Take the take out.
     * @return The take
     */
    T 𝜑();

    /**
     * End point.
     *
     * @since 0.1
     */
    final class End {
        private final Object object;
        public End(final Object src) {
            this.object = src;
        }
        @SuppressWarnings("unchecked")
        public <X> X 𝜑(final Class<X> type) {
            if (this.object instanceof Data) {
                return ((Data<X>) this.object).𝜑();
            } else {
                throw new TypeMismatchException(
                    String.format(
                        "Can't cast from %s to Data<%s>",
                        this.object.getClass(),
                        type
                    )
                );
            }
        }
    }

}
