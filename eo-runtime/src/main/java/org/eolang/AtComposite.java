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
 * Static attribute with an expression inside, which
 * constructs an object.
 *
 * @since 0.1
 */
public final class AtComposite implements Attr {

    private final Expr expr;

    private final Data<Phi> object;

    public AtComposite(final Phi rho, final Expr exp) {
        this(exp, AtComposite.toData(exp, rho));
    }

    private AtComposite(final Expr exp, final Data<Phi> data) {
        this.expr = exp;
        this.object = data;
    }

    @Override
    public String toString() {
        return "λ";
    }

    @Override
    public String φTerm() {
        return "λ";
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtComposite(
            this.expr,
            AtComposite.toData(this.expr, self)
        );
    }

    @Override
    public Phi get() {
        return this.object.take();
    }

    @Override
    public void put(final Phi phi) {
        throw new Attr.ReadOnlyException(
            "You can't overwrite static expression"
        );
    }

    /**
     * Turn ENV into DATA.
     * @param exp The \lambda expression
     * @param rho The \rho of this attribute/object
     * @return Data
     * @checkstyle IllegalCatchCheck (20 lines)
     */
    private static Data<Phi> toData(final Expr exp, final Phi rho) {
        return () -> {
            try {
                return exp.get(rho);
            } catch (final RuntimeException ex) {
                throw ex;
            } catch (final Exception ex) {
                throw new IllegalArgumentException(ex);
            }
        };
    }

}
