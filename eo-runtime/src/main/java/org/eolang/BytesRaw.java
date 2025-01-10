/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
import java.util.Arrays;

/**
 * Bytes to be created from byte array only.
 *
 * @since 0.1.0
 * @todo #3239:90min Method {@link BytesOf#shift} should be refactored to get rid of
 *  {@code @SuppressWarnings("PMD.CognitiveComplexity")} warning. You can
 *  check description of this rules here
 *  <a href="https://pmd.github.io/pmd/pmd_rules_java_design">pmd.github.io</a>
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.GodClass"})
final class BytesRaw implements Bytes {

    /**
     * Binary data.
     */
    private final byte[] data;

    /**
     * Ctor.
     * @param data Data.
     */
    BytesRaw(final byte[] data) {
        this.data = Arrays.copyOf(data, data.length);
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
    @SuppressWarnings("PMD.ShortMethodName")
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
    @SuppressWarnings("PMD.CognitiveComplexity")
    public Bytes shift(final int bits) {
        // @checkstyle MethodBodyCommentsCheck (3 lines)
        // @checkstyle NestedIfDepthCheck (40 lines)
        final byte[] bytes = this.take();
        final int mod = Math.abs(bits) % Byte.SIZE;
        final int offset = Math.abs(bits) / Byte.SIZE;
        if (bits < 0) {
            final byte carry = (byte) ((0x01 << mod) - 1);
            for (int index = 0; index < bytes.length; index += 1) {
                final int source = index + offset;
                if (source >= bytes.length) {
                    bytes[index] = 0;
                } else {
                    byte dst = (byte) (bytes[source] << mod);
                    if (source + 1 < bytes.length) {
                        dst |= (byte) (bytes[source + 1] >>> (Byte.SIZE - mod) & (carry & 0xFF));
                    }
                    bytes[index] = dst;
                }
            }
        } else {
            final byte carry = (byte) (0xFF << (Byte.SIZE - mod));
            for (int index = bytes.length - 1; index >= 0; index -= 1) {
                final int source = index - offset;
                if (source < 0) {
                    bytes[index] = 0;
                } else {
                    byte dst = (byte) ((0xff & bytes[source]) >>> mod);
                    if (source - 1 >= 0) {
                        dst |= (byte) (bytes[source - 1] << (Byte.SIZE - mod) & (carry & 0xFF));
                    }
                    bytes[index] = dst;
                }
            }
        }
        return new BytesOf(bytes);
    }

    @Override
    public Bytes sshift(final int bits) {
        if (bits < 0) {
            throw new UnsupportedOperationException(
                "The \"right shift\" is NYI"
            );
        }
        final byte[] bytes = this.shift(bits).take();
        if (this.take()[0] < 0) {
            for (int index = 0; index < bytes.length; index += 1) {
                final int zeros = BytesRaw.numberOfLeadingZeros(
                    bytes[index]
                );
                bytes[index] = (byte) (bytes[index] & 0xFF ^ (-1 << (Byte.SIZE - zeros)));
                if (zeros != Byte.SIZE) {
                    break;
                }
            }
        }
        return new BytesOf(bytes);
    }

    @Override
    public Double asNumber() {
        return this.asNumber(Double.class);
    }

    @Override
    public <T extends Number> T asNumber(final Class<T> type) {
        final byte[] ret = this.take();
        final Object res;
        final ByteBuffer buf = ByteBuffer.wrap(ret);
        if (Long.class.equals(type)) {
            res = BytesRaw.whenFit(buf, ret, Long.class).getLong();
        } else if (Integer.class.equals(type)) {
            res = BytesRaw.whenFit(buf, ret, Integer.class).getInt();
        } else if (Double.class.equals(type)) {
            res = BytesRaw.whenFit(buf, ret, Double.class).getDouble();
        } else if (Short.class.equals(type)) {
            res = BytesRaw.whenFit(buf, ret, Short.class).getShort();
        } else {
            throw new UnsupportedOperationException(
                String.format(
                    "Can't convert %d bytes to \"%s\"",
                    ret.length, type.getCanonicalName()
                )
            );
        }
        return type.cast(res);
    }

    @Override
    public String asString() {
        final StringBuilder out = new StringBuilder(0);
        for (final byte bte : this.data) {
            if (out.length() > 0) {
                out.append('-');
            }
            out.append(String.format("%02X", bte));
        }
        if (this.data.length == 0) {
            out.append("--");
        }
        return out.toString();
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

    /**
     * Count leading zero bits.
     * @param num Byte.
     * @return Number between 0 and 8.
     */
    private static byte numberOfLeadingZeros(final byte num) {
        final byte result;
        if (num == 0) {
            result = (byte) Byte.SIZE;
        } else if (num < 0) {
            result = (byte) 0;
        } else {
            byte temp = num;
            int bts = Byte.SIZE - 1;
            if (temp >= 1 << 4) {
                bts -= 4;
                temp >>>= 4;
            }
            if (temp >= 1 << 2) {
                bts -= 2;
                temp >>>= 2;
            }
            result = (byte) (bts - (temp >>> 1));
        }
        return result;
    }

    /**
     * Checks the buffer for its validity.
     * @param buf The buffer
     * @param bytes The bytes
     * @param type The type to fit into
     * @return The same buffer
     */
    private static ByteBuffer whenFit(final ByteBuffer buf, final byte[] bytes,
        final Class<?> type) {
        final int expected;
        try {
            expected = type.getField("BYTES").getInt(null);
        } catch (final NoSuchFieldException | IllegalAccessException ex) {
            throw new IllegalArgumentException(ex);
        }
        if (bytes.length != expected) {
            throw new ExFailure(
                String.format(
                    "Can't convert %d bytes to %s, exactly %d bytes expected",
                    bytes.length, type.getName(), expected
                )
            );
        }
        return buf;
    }
}
