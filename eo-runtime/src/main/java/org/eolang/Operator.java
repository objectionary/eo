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

import java.lang.reflect.InvocationTargetException;
import java.util.List;
import org.cactoos.Scalar;
import org.cactoos.list.ListOf;

/**
 * One operation.
 *
 * @since 0.1
 */
public final class Operator <D, T extends Data<D>> {

    /**
     * The start.
     */
    private final Data<D> start;

    /**
     * The start.
     */
    private final Class<T> type;

    /**
     * The reduce.
     */
    private final Operator.Reduce<D> reduce;

    /**
     * Ctor.
     * @param val The value
     * @param tpe Type of result
     * @param rdc Reduce
     */
    public Operator(final Data<D> val, final Class<T> tpe,
        final Operator.Reduce<D> rdc) {
        this.start = val;
        this.type = tpe;
        this.reduce = rdc;
    }

    /**
     * Make a copy of it.
     * @param args The args
     * @return Copy
     */
    @SafeVarargs
    public final T cp(final Data<D>... args) {
        try {
            return this.type.getConstructor(Scalar.class).newInstance(
                (Scalar<D>) () -> this.reduce.calc(
                    this.start.get(),
                    new ListOf<>(args)
                )
            );
        } catch (InstantiationException | IllegalAccessException
            | InvocationTargetException | NoSuchMethodException ex) {
            throw new IllegalStateException(ex);
        }
    }

    /**
     * Reduce action.
     *
     * @since 0.1
     */
    public interface Reduce <D> {
        /**
         * Do it.
         * @param first First value
         * @param args The args
         * @return Result
         */
        D calc(D first, List<Data<D>> args);
    }
}
