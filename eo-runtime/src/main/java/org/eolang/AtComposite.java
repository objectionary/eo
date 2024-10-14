/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

import java.util.function.Supplier;

/**
 * Attribute that constructs object lazily.
 *
 * <p>The attribute depends on context (argument of lambda expression).</p>
 *
 * @since 0.1
 */
@Versionized
public final class AtComposite implements Attr {
    /**
     * Function that returns object.
     */
    private final Supplier<Phi> func;

    /**
     * The expression itself.
     */
    private final Expr expr;

    /**
     * Ctor.
     * @param obj The \rho
     * @param exp The expression
     */
    public AtComposite(final Phi obj, final Expr exp) {
        this.expr = exp;
        this.func = new SafeFunc<>(
            () -> this.expr.get(obj)
        );
    }

    @Override
    public String toString() {
        return this.φTerm();
    }

    @Override
    public String φTerm() {
        return Attr.LAMBDA;
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtComposite(self, this.expr);
    }

    @Override
    public Phi get() {
        return this.func.get();
    }

    @Override
    public boolean put(final Phi phi) {
        throw new ExReadOnly(
            "You can't overwrite lambda expression"
        );
    }
}
