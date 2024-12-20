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

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 * Bytes.
 *
 * @since 0.1.0
 */
@SuppressWarnings("PMD.TooManyMethods")
public final class BytesOf implements Bytes {

    /**
     * Bytes.
     */
    private final Bytes bytes;

    /**
     * Ctor.
     * @param bytes Bytes.
     */
    public BytesOf(final Bytes bytes) {
        this.bytes = bytes;
    }

    /**
     * Ctor.
     * @param data Data.
     */
    public BytesOf(final byte[] data) {
        this(new BytesRaw(Arrays.copyOf(data, data.length)));
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
     * @param number Integer number.
     */
    public BytesOf(final int number) {
        this(ByteBuffer.allocate(Integer.BYTES).putInt(number).array());
    }

    /**
     * Ctor.
     * @param chr Character.
     */
    public BytesOf(final char chr) {
        this(ByteBuffer.allocate(Character.BYTES).putChar(chr).array());
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
    public BytesOf(final double number) {
        this(ByteBuffer.allocate(Double.BYTES).putDouble(number).array());
    }

    @Override
    public Bytes not() {
        return this.bytes.not();
    }

    @Override
    public Bytes and(final Bytes other) {
        return this.bytes.and(other);
    }

    @Override
    @SuppressWarnings("PMD.ShortMethodName")
    public Bytes or(final Bytes other) {
        return this.bytes.or(other);
    }

    @Override
    public Bytes xor(final Bytes other) {
        return this.bytes.xor(other);
    }

    @Override
    @SuppressWarnings("PMD.CognitiveComplexity")
    public Bytes shift(final int bits) {
        return this.bytes.shift(bits);
    }

    @Override
    public Bytes sshift(final int bits) {
        return this.bytes.sshift(bits);
    }

    @Override
    public Double asNumber() {
        return this.bytes.asNumber();
    }

    @Override
    public <T extends Number> T asNumber(final Class<T> type) {
        return this.bytes.asNumber(type);
    }

    @Override
    public String asString() {
        return this.bytes.asString();
    }

    @Override
    public byte[] take() {
        return this.bytes.take();
    }

    @Override
    public String toString() {
        return this.bytes.toString();
    }

    @Override
    public boolean equals(final Object other) {
        return this.bytes.equals(other);
    }

    @Override
    public int hashCode() {
        return this.bytes.hashCode();
    }
}
