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

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 * Bytes.
 *
 * @since 1.0
 * @todo #632:30min Implement shift & left methods.
 *  New implementation should not truncate length of
 *  the original massive. After that, update this PDD
 *  to make changes to a corresponding EObytes classes.
 */
public final class BytesOf implements Bytes {
    /**
     * Binary data
     */
    private final byte[] data;

    /**
     * Ctor.
     * @param data Data.
     */
    public BytesOf(final byte[] data) {
        this.data = data;
    }

    /**
     * Ctor.
     * @param str UTF-8 Text.
     */
    public BytesOf(final String str) {
       this(str.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Ctor.
     * @param number Long number.
     */
    public BytesOf(final long number) {
       this(ByteBuffer.allocate(Long.BYTES).putLong(number).array());
    }


    /**
     * Ctor
     * @param number Double number.
     */
    public BytesOf(final Double number) {
        this(ByteBuffer.allocate(Double.BYTES).putDouble(number).array());
    }

    @Override
    public Bytes not() {
        final byte[] copy = this.take();
        for (int i = 0; i < copy.length; i++) {
            copy[i] = (byte) ~copy[i];
        }
        return new BytesOf(copy);
    }

    @Override
    public Bytes and(final Bytes other) {
        final byte[] first = this.take();
        final byte[] second = other.take();
        for (int i = 0; i < Math.min(first.length, second.length); i++) {
            first[i] = (byte) (first[i] & second[i]);
        }
        return new BytesOf(first);
    }

    @Override
    public Bytes or(final Bytes other) {
        final byte[] first = this.take();
        final byte[] second = other.take();
        for (int i = 0; i < Math.min(first.length, second.length); i++) {
            first[i] = (byte) (first[i] | second[i]);
        }
        return new BytesOf(first);
    }

    @Override
    public Bytes xor(final Bytes other) {
        final byte[] first = this.take();
        final byte[] second = other.take();
        for (int i = 0; i < Math.min(first.length, second.length); i++) {
            first[i] = (byte) (first[i] ^ second[i]);
        }
        return new BytesOf(first);
    }

    @Override
    public Bytes left(final int size) {
        throw new UnsupportedOperationException("left-shift in NYI");
    }

    @Override
    public Bytes right(final int size) {
        throw new UnsupportedOperationException("right-shift in NYI");
    }

    @Override
    public <T extends Number> T asNumber(final Class<T> type) {
        final byte[] ret = this.take();
        final Object res;
        final ByteBuffer buf = ByteBuffer.wrap(ret);
        if (Long.class.equals(type)
            && ret.length == Long.BYTES) {
            res = buf.getLong();
        } else if (Integer.class.equals(type)
            && ret.length == Integer.BYTES
        ) {
            res = buf.getLong();
        } else if (Double.class.equals(type)
            && ret.length == Double.BYTES
        ) {
            res = buf.getDouble();
        } else {
            throw new UnsupportedOperationException(
                String.format(
                    "Unsupported conversion to '%s' for %d bytes",
                    type,
                    ret.length
                )
            );
        }
        return type.cast(res);
    }

    @Override
    public byte[] take() {
        return Arrays.copyOf(this.data, this.data.length);
    }

    @Override
    public String toString() {
        return String.format("BytesOf{%s}", Arrays.toString(this.data));
    }

    @Override
    public boolean equals(final Object other) {
        final boolean result;
        if (this == other) {
            result = true;
        } else if (other instanceof Bytes) {
            result = Arrays.equals(this.data, ((Bytes) other).take());
        } else {
            result = false;
        }
        return result;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(this.data);
    }
}
