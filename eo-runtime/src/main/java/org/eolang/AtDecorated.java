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

import java.util.function.Supplier;

/**
 * When a child object is taken from the \phi object, this class
 * replaces the \rho attribute of it on the fly. This is necessary
 * for this scenario, for example:
 *
 * [] > parent
 *   memory TRUE > toggle
 *   toggle.while > @
 *     [x] > kid
 *       ^.^.write FALSE
 *
 * Here, the "while" object has TRUE as its \rho, which would make
 * this loop endless: the "while" will always read from the TRUE constant.
 * Thus, we set \rho to the object while to "toggle".
 *
 * @since 0.16
 */
final class AtDecorated implements Attr {

    /**
     * The \phi attribute.
     */
    private final Attr phi;

    /**
     * The name.
     */
    private final String name;

    /**
     * The owner of the \phi attribute.
     */
    private final Supplier<Phi> rho;

    /**
     * Ctor.
     * @param aphi The \phi
     * @param attr The origin
     * @param arho The owner of the \phi attribute
     */
    AtDecorated(final Attr aphi, final String attr, final Supplier<Phi> arho) {
        this.phi = aphi;
        this.name = attr;
        this.rho = arho;
    }

    @Override
    public String toString() {
        return String.format("%sD", this.name);
    }

    @Override
    public String φTerm() {
        return String.format("%s.%s", this.phi.φTerm(), this.name);
    }

    @Override
    public Attr copy(final Phi obj) {
        return this;
    }

    @Override
    public Phi get() {
        final Phi base = this.phi.get();
        return base.attr(this.name).copy(base).get().copy(this.rho.get());
    }

    @Override
    public void put(final Phi obj) {
        throw new Attr.ReadOnlyException(
            String.format(
                "It's not allowed to set attribute '%s' of a decoratee",
                this.name
            )
        );
    }
}
