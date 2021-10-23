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
     * The \phi attribute this child came from.
     */
    private final Attr phi;

    /**
     * The self.
     */
    private final Phi self;

    /**
     * The name of the attribute to take from \phi.
     */
    private final String attr;

    /**
     * The parent to put into \rho attribute of the original object.
     */
    private final Phi parent;

    /**
     * Ctor.
     *
     * @param aphi The \phi attribute this child came from
     * @param name The attr
     * @param slf Self
     * @param prnt The value of \rho to use
     */
    AtDecorated(final Attr aphi, final String name, final Phi slf, final Phi prnt) {
        this.phi = aphi;
        this.attr = name;
        this.self = slf;
        this.parent = prnt;
    }

    @Override
    public Attr copy(final Phi slf) {
        return new AtDecorated(this.phi.copy(slf), this.attr, slf, this.parent);
    }

    @Override
    public Phi get() {
        final Phi inner = this.phi.get().attr(this.attr).copy(this.self).get();
        if (!(inner instanceof Data)) {
            inner.attr("ρ").put(this.parent);
        }
        return inner;
    }

    @Override
    public void put(final Phi obj) {
        this.phi.get().attr(this.attr).copy(this.self).put(obj);
    }
}
