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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import org.cactoos.Scalar;
import org.cactoos.scalar.Unchecked;

/**
 * BOOLEAN.
 *
 * @since 0.1
 */
public class EObool implements Data<Boolean> {

    /**
     * The value.
     */
    private final Unchecked<Boolean> value;

    /**
     * Ctor.
     * @param val The value
     */
    public EObool(final boolean val) {
        this(() -> val);
    }

    /**
     * Ctor.
     * @param val The value
     */
    public EObool(final Scalar<Boolean> val) {
        this.value = new Unchecked<>(val);
    }

    @Override
    public final Boolean get() {
        return this.value.value();
    }

    /**
     * Makes a fork.
     *
     * new EObool(true).if().copy("left", "right")
     *
     * @param <T> Type of result
     * @return Result
     * @checkstyle NonStaticMethodCheck (3 lines)
     */
    public final <T> EObool.OpIf<T> eoif() {
        return new EObool.OpIf<>(this.value);
    }

    /**
     * Operator IF.
     *
     * @param <T> The type of return
     * @since 0.1
     */
    @SuppressWarnings("unchecked")
    public final class OpIf<T> extends EObool {
        /**
         * Ctor.
         * @param val The value
         */
        public OpIf(final Scalar<Boolean> val) {
            super(val);
        }
        /**
         * Make a copy.
         *
         * @param left Left branch (positive)
         * @param right Right branch (negative)
         * @return Data with result
         * @checkstyle MethodNameCheck (3 lines)
         */
        public Data<T> copy(final Data<?> left, final Data<?> right) {
            try {
                final Constructor<Data<T>> ctor =
                    (Constructor<Data<T>>) left.getClass()
                        .getDeclaredConstructor(Scalar.class);
                return ctor.newInstance(
                    (Scalar<T>) () -> {
                        final T result;
                        if (this.get()) {
                            result = (T) left.get();
                        } else {
                            result = (T) right.get();
                        }
                        return result;
                    }
                );
            } catch (final NoSuchMethodException | IllegalAccessException
                | InstantiationException | InvocationTargetException ex) {
                throw new IllegalStateException(
                    String.format(
                        "Can't make a copy of %s",
                        left.getClass()
                    ),
                    ex
                );
            }
        }
    }
}
