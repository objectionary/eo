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
 *         "plus",
 *         "x",
 *         new ExrpReduce.Args(
 *             Long.class,
 *             Long::sum,
 *             x -> ""
 *         )
 *     );
 * </pre></code>
 * @param <T> Type of arguments passing to validation and reduction functions
 * @since 1.0
 */
public final class ExprReduce<T> implements Expr {

    /**
     * Param name with varargs.
     */
    private final String param;

    /**
     * Object contains type of varargs, validation and reduction functions.
     */
    private final Args<T> arguments;

    /**
     * Operation name.
     */
    private final String oper;

    /**
     * Ctor.
     *
     * @param oper Operation name
     * @param param Param name with varargs
     * @param arguments Object contains type of varargs, validation and reduction functions
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
        T acc = this.arguments.accumulator(rho);
        final Phi[] args = new Param(rho, this.param).strong(Phi[].class);
        for (int idx = 0; idx < args.length; ++idx) {
            final Object val = new Dataized(args[idx]).take();
            this.arguments.checkType(val, this.oper, idx);
            final String msg = this.arguments.validate(val);
            if (!msg.isEmpty()) {
                throw new ExFailure(
                    "The %dth argument of '%s' is invalid: %s",
                    idx + 1, this.oper, msg
                );
            }
            acc = this.arguments.reduce(acc, val);
        }
        return new Data.ToPhi(acc);
    }

    /**
     * Builds a class that stores some attributes of ExprReduce object.
     * <p/>Definition example:
     * <code><pre>
     *     final VarargExpr&lt;Long&gt; st = new Args<>(
     *         Long.class,
     *         (acc, x) -> acc / x,
     *         x -> {
     *             String msg = "";
     *             if (x.equals(0.0)) {
     *                 msg = "division by zero is infinity";
     *             }
     *             return msg;
     *         }
     *     );
     * </pre></code>
     * @param <T> Type of arguments passing to validation and reduction functions
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
         * @param type Type of varargs
         * @param reduction Reduction operation on consecutive varags
         * @param validation Validation function
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

        /**
         * Ctor.
         *
         * @param val New object that should be added
         * @param oper Operation name
         * @param idx Index or the currently processed argument
         */
        public void checkType(final Object val, final String oper, final int idx) {
            if (!val.getClass().getCanonicalName().equals(this.type.getCanonicalName())) {
                throw new ExFailure(
                    "The %dth argument of '%s' is not a(n) %s: %s",
                    idx + 1, oper, this.type.getSimpleName(), val
                );
            }
        }

        /**
         * Ctor.
         *
         * @param rho Rho object
         * @return Retuns the default state of accumulator
         */
        public T accumulator(final Phi rho) {
            return new Param(rho).strong(this.type);
        }

        /**
         * Ctor.
         *
         * @param val New object that should be added
         * @return Returns empty string if validation passed and error message else
         */
        public String validate(final Object val) {
            final T typed = this.type.cast(val);
            return this.validation.apply(typed);
        }

        /**
         * Ctor.
         *
         * @param acc Accumulator of the reduce method
         * @param val New object that should be added
         * @return Returns new state of accumulator after adding a new value
         */
        public T reduce(final T acc, final Object val) {
            final T typed = this.type.cast(val);
            return this.reduction.apply(acc, typed);
        }
    }
}
