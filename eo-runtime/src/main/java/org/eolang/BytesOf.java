/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
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

    /**
     * Ctor.
     * @param data Data.
     */
    public BytesOf(final byte[] data) {
        this(new BytesRaw(Arrays.copyOf(data, data.length)));
    }

    /**
     * Ctor.
     * @param bytes Bytes.
     */
    public BytesOf(final Bytes bytes) {
        this.bytes = bytes;
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
