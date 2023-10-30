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

/**
 * Attribute with lambda expression inside.
 *
 * @since 0.33.0
 */
@Versionized
public final class AtLambda implements Attr {

    /**
     * The \rho to send to the expression.
     */
    private final Phi rho;

    /**
     * The expression itself.
     */
    private final Expr expr;

    /**
     * Ctor.
     * @param obj The \rho
     * @param exp The expression
     */
    public AtLambda(final Phi obj, final Expr exp) {
        this.rho = obj;
        this.expr = exp;
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
        return new AtLambda(self, this.expr);
    }

    @Override
    public Phi get() {
        try {
            return this.expr.get(this.rho);
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new ExInterrupted();
        // @checkstyle IllegalCatchCheck (3 line)
        } catch (final RuntimeException ex) {
            throw ex;
        } catch (final Throwable ex) {
            throw new ExFailure(
                String.format(
                    "Unexpected error '%s' of type %s",
                    ex.getMessage(),
                    ex.getClass().getSimpleName()
                ),
                ex
            );
        }
    }

    @Override
    public void put(final Phi phi) {
        throw new ExReadOnly(
            "You can't overwrite labmda expression"
        );
    }
}
