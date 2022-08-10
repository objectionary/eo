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

import java.math.BigInteger;
import java.nio.ByteBuffer;

/**
 * Param of an object (convenient retrieval mechanism).
 *
 * <p>The job of the utility object is to help our EO objects retrieve
 * attributes from other objects and from their own \rho (owners). On top of
 * retrieval this object also does simple type checking. When an attribute
 * is expected to be of some type, we use {@link #strong(Class)}. This method
 * will throw a runtime exception if types don't match. If just a simple
 * retrieval without type checking is necessary, just use the method
 * {@link #weak()}.
 *
 * @since 0.20
 */
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
        final Object ret = this.weak();
        if (!type.isInstance(ret)) {
            throw new ExFailure(
                String.format(
                    "The argument '.%s' is of Java type '%s', not '%s' as expected",
                    this.attr,
                    ret.getClass().getCanonicalName(),
                    type.getCanonicalName()
                )
            );
        }
        return type.cast(ret);
    }

    /**
     * Fetch and DON'T check type.
     * @return The object
     */
    public Object weak() {
        return new Dataized(
            this.rho.attr(this.attr).get()
        ).take();
    }

    /**
     * Fetch BYTES as type.
     * @param type The type
     * @param <T> The type
     * @return The object
     */
    public <T> T fromBytes(final Class<T> type) {
        final byte[] ret = this.strong(byte[].class);
        final Object res;
        if (BigInteger.class.equals(type)) {
            res = new BigInteger(ret);
        } else if (Long.class.equals(type)) {
            if (ret.length == 1) {
                res = (long) ret[0];
            } else {
                final byte[] cpy = new byte[Long.BYTES];
                int posx = cpy.length;
                int posy = ret.length;
                while (posy > 0 && posx > 0) {
                    posy -= 1;
                    posx -= 1;
                    cpy[posx] = ret[posy];
                }
                while (ret[0] < 0 && posx > 0) {
                    posx -= 1;
                    cpy[posx] = -1;
                }
                res = ByteBuffer.wrap(cpy).getLong();
            }
        } else if (Character.class.equals(type)) {
            res = ByteBuffer.wrap(ret).getChar();
        } else if (Double.class.equals(type)) {
            res = ByteBuffer.wrap(ret).getDouble();
        } else {
            throw new ExFailure(
                String.format(
                    "Unsupported type: '%s'",
                    type
                )
            );
        }
        return type.cast(res);
    }

    /**
     * Fetch BYTES of any type.
     * @return The bytes.
     */
    public byte[] asBytes() {
        final Object ret = this.weak();
        final byte[] res;
        if (Long.class.isInstance(ret)) {
            res = ByteBuffer.allocate(Long.BYTES).putLong((long) ret).array();
        } else if (Character.class.isInstance(ret)) {
            res = ByteBuffer.allocate(Character.BYTES).putChar((char) ret).array();
        } else if (Double.class.isInstance(ret)) {
            res = ByteBuffer.allocate(Double.BYTES).putDouble((double) ret).array();
        } else {
            throw new ExFailure(
                String.format(
                    "Unsupported type: %s", ret.getClass()
                )
            );
        }
        return res;
    }
}
