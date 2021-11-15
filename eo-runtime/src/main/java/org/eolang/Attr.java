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
 * Attribute.
 *
 * @since 0.1
 */
public interface Attr extends Term {

    /**
     * Make a copy of it.
     *
     * @param self The object that this attribute will belong to
     * @return A copy
     */
    Attr copy(Phi self);

    /**
     * Take the object out.
     *
     * @return The object
     */
    Phi get();

    /**
     * Put a new object in.
     *
     * @param phi The object to put
     */
    void put(Phi phi);

    /**
     * The exception raised when something is not right inside
     * attributes.
     *
     * @since 0.1
     */
    final class IllegalAttrException extends RuntimeException {

        private static final long serialVersionUID = 597749420437007615L;

        public IllegalAttrException(final String cause) {
            super(cause);
        }

        public IllegalAttrException(final String cause, final Throwable root) {
            super(cause, root);
        }
    }

    /**
     * The exception raised when trying to get() an attribute,
     * which is still abstract.
     *
     * @since 0.13
     */
    final class StillAbstractException extends RuntimeException {

        private static final long serialVersionUID = 597748420437017615L;

        public StillAbstractException(final String cause) {
            super(cause);
        }

        public StillAbstractException(final String cause, final Throwable root) {
            super(cause, root);
        }
    }

    /**
     * The exception raised when trying to put() an attribute,
     * which is read-only.
     *
     * @since 0.13
     */
    final class ReadOnlyException extends RuntimeException {

        private static final long serialVersionUID = 697748420437017615L;

        public ReadOnlyException(final String cause) {
            super(cause);
        }

        public ReadOnlyException(final String cause, final Throwable root) {
            super(cause, root);
        }
    }

}
