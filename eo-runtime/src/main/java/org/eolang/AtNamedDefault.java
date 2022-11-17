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

/**
 * Make named attribute.
 *
 * @since 0.1
 */
public final class AtNamedDefault implements Attr {

    /**
     * The named attr.
     */
    private final AtNamed named;

    /**
     * Ctor.
     *
     * @param origin The original attr
     * @param name The name of attr
     * @param phi The phi for oname instance
     */
    public AtNamedDefault(final Attr origin, final String name, final Phi phi) {
        this.named = new AtNamed(
            String.format(
                "%s#%s",
                phi.getClass().getCanonicalName(),
                name
            ),
            String.format(
                "%s.%s",
                new Oname(phi),
                name
            ),
            phi,
            origin
        );
    }

    /**
     * Reproduce value as {@link AtNamed}.
     *
     * @return Named attr
     */
    public AtNamed value() {
        return this.named;
    }

    @Override
    public Attr copy(final Phi self) {
        return this.named.copy(self);
    }

    @Override
    public Phi get() {
        return this.named.get();
    }

    @Override
    public void put(final Phi phi) {
        this.named.put(phi);
    }

    @Override
    public String φTerm() {
        return this.named.φTerm();
    }
}
