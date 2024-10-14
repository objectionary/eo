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

/**
 * The attribute tries to copy object and set \rho to it.
 *
 * <p>If the name of the attribute is {@link Attr#RHO} - just object is returned.</p>
 *
 * @since 0.36.0
 */
final class AtSetRho extends AtEnvelope {
    /**
     * Ctor.
     * @param obj Object
     * @param rho Rho that will be set
     * @param name Name of the attribute
     */
    AtSetRho(final Phi obj, final Phi rho, final String name) {
        this(new AtSimple(obj), rho, name);
    }

    /**
     * Ctor.
     * @param attr Origin attribute
     * @param rho Rho that will be set
     * @param name Name of the attribute
     */
    AtSetRho(final Attr attr, final Phi rho, final String name) {
        super(
            new AtGetOnly(
                () -> {
                    Phi ret = attr.get();
                    if (!name.equals(Attr.RHO)) {
                        final Phi copy = ret.copy();
                        if (copy.put(Attr.RHO, rho)) {
                            ret = copy;
                        }
                    }
                    return ret;
                }
            )
        );
    }
}
