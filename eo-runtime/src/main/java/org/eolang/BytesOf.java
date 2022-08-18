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
 * @todo #632:30min Current shift implementation
 *  does not preserve sing. Add sign-preserving
 *  shift if needed. After that, update this PDD
 *  to make changes to corresponding EObytes classes.
 */
public final class BytesOf implements Bytes {

    /**
     * Binary data.
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
     * Ctor.
     * @param number Double number.
     */
    public BytesOf(final Double number) {
        this(ByteBuffer.allocate(Double.BYTES).putDouble(number).array());
    }

    @Override
    public Bytes not() {
        final byte[] copy = this.take();
        for (int index = 0; index < copy.length; index += 1) {
            copy[index] = (byte) ~copy[index];
        }
        return new BytesOf(copy);
    }

    @Override
    public Bytes and(final Bytes other) {
        final byte[] first = this.take();
        final byte[] second = other.take();
        for (int index = 0; index < Math.min(first.length, second.length); index += 1) {
            first[index] = (byte) (first[index] & second[index]);
        }
        return new BytesOf(first);
    }

    @Override
    public Bytes or(final Bytes other) {
        final byte[] first = this.take();
        final byte[] second = other.take();
        for (int index = 0; index < Math.min(first.length, second.length); index += 1) {
            first[index] = (byte) (first[index] | second[index]);
        }
        return new BytesOf(first);
    }

    @Override
    public Bytes xor(final Bytes other) {
        final byte[] first = this.take();
        final byte[] second = other.take();
        for (int index = 0; index < Math.min(first.length, second.length); index += 1) {
            first[index] = (byte) (first[index] ^ second[index]);
        }
        return new BytesOf(first);
    }

    @Override
    public Bytes shift(final int bits) {
        // @checkstyle MethodBodyCommentsCheck (3 lines)
        // @checkstyle NestedIfDepthCheck (40 lines)
        final byte[] bytes = this.take();
        final int mod = Math.abs(bits) % 8;
        final int offset = Math.abs(bits) / 8;
        if (bits < 0) {
            final byte carry = (byte) ((0x01 << mod) - 1);
            for (int index = 0; index < bytes.length; index += 1) {
                final int source = index + offset;
                if (source >= bytes.length) {
                    bytes[index] = 0;
                } else {
                    byte dst = (byte) (bytes[source] << mod);
                    if (source + 1 < bytes.length) {
                        dst |= bytes[source + 1] >>> (8 - mod) & carry;
                    }
                    bytes[index] = dst;
                }
            }
        } else {
            final byte carry = (byte) (0xFF << (8 - mod));
            for (int index = bytes.length - 1; index >= 0; index -= 1) {
                final int source = index - offset;
                if (source < 0) {
                    bytes[index] = 0;
                } else {
                    byte dst = (byte) ((0xff & bytes[source]) >>> mod);
                    if (source - 1 >= 0) {
                        dst |= bytes[source - 1] << (8 - mod) & carry;
                    }
                    bytes[index] = dst;
                }
            }
        }
        return new BytesOf(bytes);
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
