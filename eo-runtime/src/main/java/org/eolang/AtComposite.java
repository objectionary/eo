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
 * Static attribute with an expression inside, which
 * constructs an object.
 *
 * @since 0.1
 * @todo #2566:60min Remove AtComposite class. AtComposite duplicates the functionality of
 *  {@link AtLambda} and it's used only because the style of generated (from EO) java is
 *  imperative. We need to make transpilation declarative (for example
 *  new PhLocated(new PhWith(new PhMethod(...), ...), ...)) so we would not need AtComposite
 *  anymore. Don't forget to remove the puzzle.
 */
@Versionized
public final class AtComposite implements Attr {
    /**
     * Original attribute.
     */
    private final Attr origin;

    /**
     * Ctor.
     * @param obj The \rho
     * @param exp The expression
     */
    public AtComposite(final Phi obj, final Expr exp) {
        this(new AtLambda(obj, exp));
    }

    /**
     * Ctor.
     * @param attr Attribute.
     */
    AtComposite(final Attr attr) {
        this.origin = attr;
    }

    @Override
    public String toString() {
        return this.origin.toString();
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtComposite(this.origin.copy(self));
    }

    @Override
    public Phi get() {
        return this.origin.get();
    }

    @Override
    public void put(final Phi phi) {
        this.origin.put(phi);
    }
}
