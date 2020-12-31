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

package org.eolang.phi;

/**
 * A data container.
 *
 * @since 0.1
 */
public interface Data<T> {

    T take();

    final class Value<T> extends PhDefault implements Data<T> {
        private final T val;
        public Value(final T value) {
            super(Phi.ETA);
            this.val = value;
        }
        @Override
        public String toString() {
            return this.val.toString();
        }
        @Override
        public T take() {
            return this.val;
        }
    }

    final class Take {
        private final Phi phi;
        public Take(final Phi src) {
            this.phi = src;
        }
        public Object take() {
            Phi src = this.phi;
            if (!(src instanceof Data)) {
                src = src.attr("data").get();
            }
            return Data.class.cast(src).take();
        }
        public <T> T take(final Class<T> type) {
            return type.cast(this.take());
        }
    }

}
