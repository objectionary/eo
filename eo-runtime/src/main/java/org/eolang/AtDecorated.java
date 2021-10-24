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
 * When a child object is taken from the \phi object, this class
 * replaces the \rho attribute of it on the fly.
 *
 * @since 0.16
 */
final class AtDecorated implements Attr {
    /**
     * The \phi attribute.
     */
    private final Attr base;

    /**
     * The name.
     */
    private final String name;

    /**
     * The self.
     */
    private final Phi self;

    /**
     * Ctor.
     * @param phi The \phi owner of this one
     * @param attr The origin
     * @param slf The self
     */
    AtDecorated(final Attr phi, final String attr, final Phi slf) {
        this.base = phi;
        this.name = attr;
        this.self = slf;
    }

    @Override
    public String toString() {
        return String.format("%sD", this.name);
    }

    @Override
    public Attr copy(final Phi slf) {
        return new AtDecorated(this.base.copy(slf), this.name, slf);
    }

    @Override
    public Phi get() {
        return this.base.get().attr(this.name).get();
    }

    @Override
    public void put(final Phi phi) {
        throw new Attr.ReadOnlyException(
            String.format(
                "It's not allowed to set attribute '%s' of a decoratee",
                this.name
            )
        );
    }
}
