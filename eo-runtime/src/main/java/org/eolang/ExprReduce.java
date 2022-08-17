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

import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Builds a phi performing reduction operation on varargs parameter.
 * <br/>The expression iterates over varargs (including rho)
 * one by one performing provided reduction operation.
 * Type checking is done for each vararg.
 * <p/>Definition example:
 * <code><pre>
 *     final VarargExpr&lt;Long&gt; expr = new VarargExpr<>(
 *         new ExprReduce.Storage(
 *         "plus",
 *         "x",
 *         Long.class
 *         ),
 *         Long::sum,
 *     );
 * </pre></code>
 * @param <T> Type of arguments
 * @since 1.0
 */
public final class ExprReduce<T> implements Expr {

    /**
     * Param name with varargs.
     */
    private final String param;

    private final Args<T> arguments;

    /**
     * Operation name.
     */
    private final String oper;

    /**
     * Ctor.
     *
     * @param arguments Stores operation name, name of parameter with varargs and type of varargs
     * @param reduction Reduction operation on consecutive varags
     * @param validation Validation function
     */
    public ExprReduce(
            final String oper,
            final String param,
            final Args<T> arguments
    ) {
        this.oper = oper;
        this.param = param;
        this.arguments = arguments;
    }

    @Override
    public Phi get(final Phi rho) {
        T acc = arguments.accumulator(rho);
        final Phi[] args = new Param(rho, this.param).strong(Phi[].class);
        for (int idx = 0; idx < args.length; ++idx) {
            final Object val = new Dataized(args[idx]).take();
            arguments.checkType(val,this.oper, idx);
            final String msg = arguments.validate(val);
            if (!msg.isEmpty()) {
                throw new ExFailure(
                        "The %dth argument of '%s' is invalid: %s",
                        idx + 1, this.oper, msg
                );
            }
            acc = arguments.reduce(acc, val);
        }
        return new Data.ToPhi(acc);
    }

    /**
     * Builds a class that stores some attributes of ExprReduce object.
     * <p/>Definition example:
     * <code><pre>
     *     final VarargExpr&lt;Long&gt; st = new Storage<>(
     *         "plus",
     *         "x",
     *         Long.class
     *     );
     * </pre></code>
     * @param <T> Type of arguments
     * @since 1.0
     */
    public static final class Args<T> {

        /**
         * Varargs type.
         */
        private final Class<T> type;

        /**
         * Reduction.
         */
        private final BiFunction<T, T, T> reduction;

        /**
         * Validation.
         */
        private final Function<T, String> validation;

        /**
         * Ctor.
         *
         * @param oper Operation name
         * @param param Name of parameter with varargs
         * @param type Type of varargs
         */
        public Args(
                final Class<T> type,
                final BiFunction<T, T, T> reduction,
                final Function<T, String> validation
        ) {
            this.type = type;
            this.reduction = reduction;
            this.validation = validation;
        }

        public void checkType(final Object val, final String oper, int idx){
            if (!val.getClass().getCanonicalName().equals(this.type.getCanonicalName())) {
                throw new ExFailure(
                        "The %dth argument of '%s' is not a(n) %s: %s",
                        idx + 1, oper, this.type.getSimpleName(), val
                );
            }
        }

        public T accumulator(final Phi rho){
            return new Param(rho).strong(this.type);
        }

        public String validate(final Object val){
            final T typed = this.type.cast(val);
            return this.validation.apply(typed);
        }

        public T reduce(T acc, final Object val){
            final T typed = this.type.cast(val);
            return this.reduction.apply(acc, typed);
        }
    }
}