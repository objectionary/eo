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
 * Param of an object (convenient retrieval mechanism).
 *
 * <p>The job of the utility object is to help our EO objects retrieve
 * attributes from other objects and from their own \rho (owners). On top of
 * retrieval, this object also does simple type checking. When an attribute
 * is expected to be of some type, we use {@link #strong(Class)}. This method
 * will throw a runtime exception if types don't match.
 * @since 0.20
 */
@Versionized
public final class Param {

    /**
     * The object.
     */
    private final Phi rho;

    /**
     * Attr name.
     */
    private final String attr;

    /**
     * Ctor.
     * @param obj The object to fetch \rho from
     */
    public Param(final Phi obj) {
        this(obj, "ρ");
    }

    /**
     * Ctor.
     * @param obj The object to fetch the attribute from
     * @param name Name of the attr
     */
    public Param(final Phi obj, final String name) {
        this.rho = obj;
        this.attr = name;
    }

    /**
     * Fetch and check type.
     * @param type The type
     * @param <T> The type
     * @return The object
     */
    public <T> T strong(final Class<T> type) {
        return this.dataized().take(type);
    }

    /**
     * Fetch BYTES of any type.
     * @return Bytes.
     */
    public Bytes asBytes() {
        return new BytesOf(this.dataized().take(byte[].class));
    }

    /**
     * Fetch and DON'T check type.
     * @return The object
     */
    private Dataized dataized() {
        return new Dataized(
            this.rho.attr(this.attr).get()
        );
    }
}
