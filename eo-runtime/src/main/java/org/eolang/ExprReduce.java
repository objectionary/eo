/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BinaryOperator;
import java.util.function.Function;

/**
 * Builds a phi performing reduction operation on varargs parameter.
 * <p/>Definition example:
 * <code><pre>
 *     final VarargExpr&lt;Long&gt; expr = new VarargExpr<>(
 *         "x",
 *         new ExrpReduce.Args(
 *             "plus",
 *             Long.class,
 *             Long::sum,
 *             x -> ""
 *         )
 *     );
 * </pre></code>
 * @param <T> Type of arguments that are going to be reduced
 * @since 1.0
 */
public final class ExprReduce<T> implements Expr {

    /**
     * Param name with varargs.
     */
    private final String param;

    /**
     * Reduction.
     */
    private final BinaryOperator<T> reduction;

    /**
     * Arguments parsing class.
     */
    private final Args<T> arguments;

    /**
     * Ctor.
     *
     * @param param Name of parameter with varargs
     * @param reduction Reduction operation on consecutive varags
     * @param arguments Arguments storing and parsing object
     * @since 1.0
     */
    public ExprReduce(
        final String param,
        final BinaryOperator<T> reduction,
        final Args<T> arguments
    ) {
        this.param = param;
        this.reduction = reduction;
        this.arguments = arguments;
    }

    @Override
    public Phi get(final Phi rho) {
        final Optional<T> acc = this.arguments.get(rho, this.param).stream().reduce(this.reduction);
        if (!acc.isPresent()) {
            throw new IllegalStateException(
                String.format(
                    "Unable to reduce expression for %s",
                    rho
                )
            );
        }
        return new Data.ToPhi(acc.get());
    }

    /**
     * Extracts and validates args.
     * <br/>The expression iterates over varargs (including rho)
     * one by one performing provided reduction operation.
     * Type checking is done for each vararg.
     * <p/>Definition example:
     * <code><pre>
     *     final VarargExpr&lt;Long&gt; args = new Args<>(
     *         "plus",
     *         Long.class,
     *         Long::sum,
     *         x -> ""
     *
     *     );
     * </pre></code>
     * @param <T> Type of arguments that are going to be reduced
     * @since 1.0
     */
    public static final class Args<T> {

        /**
         * Varargs type.
         */
        private final Class<T> type;

        /**
         * Validation.
         */
        private final Function<T, String> validation;

        /**
         * Operation name.
         */
        private final String oper;

        /**
         * Ctor.
         *
         * @param type Type of parameter with varargs
         * @param validation Validation operation on varargs
         * @param oper Operation that that should be used with varargs
         */
        public Args(
            final Class<T> type,
            final Function<T, String> validation,
            final String oper
        ) {
            this.type = type;
            this.validation = validation;
            this.oper = oper;
        }

        /**
         * Ctor.
         *
         * @param rho Rho argument that is parsed
         * @param param Name of parameter with varargs
         * @return Returns the list of parsed and validated values
         */
        public List<T> get(final Phi rho, final String param) {
            final T acc = new Param(rho).strong(this.type);
            final Phi[] args = new Param(rho, param).strong(Phi[].class);
            final List<T> list = new ArrayList<>(args.length + 1);
            list.add(acc);
            for (int idx = 0; idx < args.length; ++idx) {
                final Object val = new Dataized(args[idx]).take();
                if (!val.getClass().getCanonicalName().equals(this.type.getCanonicalName())) {
                    throw new ExFailure(
                        "The %dth argument of '%s' is not a(n) %s: %s",
                        idx + 1, this.oper, this.type.getSimpleName(), val
                    );
                }
                final T typed = this.type.cast(val);
                final String msg = this.validation.apply(typed);
                if (!msg.isEmpty()) {
                    throw new ExFailure(
                        "The %dth argument of '%s' is invalid: %s",
                        idx + 1, this.oper, msg
                    );
                }
                list.add(typed);
            }
            return list;
        }

    }
}
