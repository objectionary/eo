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
 *         Long.class,
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
     * Operation name.
     */
    private final String oper;

    /**
     * Ctor.
     *
     * @param oper Operation name
     * @param param Name of parameter with varargs
     * @param type Type of varargs
     * @param reduction Reduction operation on consecutive varags
     * @param validation Validation function
     */
    public ExprReduce(
        final String oper,
        final String param,
        final Class<T> type,
        final BiFunction<T, T, T> reduction,
        final Function<T, String> validation
    ) {
        this.param = param;
        this.type = type;
        this.reduction = reduction;
        this.oper = oper;
        this.validation = validation;
    }

    /**
     * Ctor.
     * @param oper Peration name
     * @param param Name of parameter with varargs
     * @param type Type of varargs
     * @param reduction Reduction operation on consecutive varargs
     */
    public ExprReduce(
        final String oper,
        final String param,
        final Class<T> type,
        final BiFunction<T, T, T> reduction
    ) {
        this(
            oper,
            param,
            type,
            reduction,
            x -> ""
        );
    }

    @Override
    public Phi get(final Phi rho) {
        T acc = new Param(rho).strong(this.type);
        final Phi[] args = new Param(rho, this.param).strong(Phi[].class);
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
            acc = this.reduction.apply(acc, typed);
        }
        return new Data.ToPhi(acc);
    }
}
