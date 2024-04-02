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

/*
 * @checkstyle PackageNameCheck (4 lines)
 */

package org.eolang;

import java.util.function.Function;

/**
 * Object that writes other object to own \rho.
 * (see {@link EOorg.EOeolang.EOcage$EOnew} and {@link EOorg.EOeolang.EOmemory$EOalloc}).
 * @since 0.36.0
 */
public final class PhWrite extends PhDefault implements Atom {
    /**
     * Attribute name.
     */
    private final String attribute;

    /**
     * Return value.
     */
    private final Function<Phi, Phi> value;

    /**
     * Ctor.
     * @param sigma Sigma
     * @param attr Attribute name
     * @param ret Return value function
     */
    public PhWrite(
        final Phi sigma,
        final String attr,
        final Function<Phi, Phi> ret
    ) {
        super(sigma);
        this.attribute = attr;
        this.add(this.attribute, new AtVoid(this.attribute));
        this.value = ret;
    }

    @Override
    public Phi lambda() {
        final Phi rho = this.take(Attr.RHO);
        rho.put(this.attribute, this.take(this.attribute));
        return this.value.apply(rho);
    }
}
