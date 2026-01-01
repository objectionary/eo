/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Bytes.
 *
 * <p>Represents byte array of arbitrary size,
 * convertible to a numeric value.
 * Original size is preserved by and, or and xor.</p>
 *
 * @since 0.1.0
 */
public interface Bytes {
    /**
     * NOT operation.
     * @return Bytes.
     */
    Bytes not();

    /**
     * AND operation.
     * @param other Bytes.
     * @return Bytes.
     */
    Bytes and(Bytes other);

    /**
     * OR operation.
     * @param other Bytes.
     * @return Bytes.
     * @checkstyle MethodNameCheck (3 lines)
     */
    @SuppressWarnings("PMD.ShortMethodName")
    Bytes or(Bytes other);

    /**
     * XOR operation.
     * @param other Bytes.
     * @return Bytes.
     */
    Bytes xor(Bytes other);

    /**
     * Big-endian unsigned shift.
     * Shifts left if value is positive, or right otherwise.
     * Does not perform sign extension.
     *
     * @param bits Bits to shift, negative to shift left.
     * @return Bytes.
     */
    Bytes shift(int bits);

    /**
     * Big-endian signed right shift.
     * Performs sign extension, i.e. it will
     * fill the top bits with 1 if the first bit is 1
     * and with 0 otherwise.
     *
     * @param bits Bits to shift, negative value causes exception.
     * @return Bytes.
     */
    Bytes sshift(int bits);

    /**
     * Convert to double number.
     * @return Number.
     */
    Double asNumber();

    /**
     * Convert to number.
     * @param type Number type
     * @param <T> Type
     * @return Number
     */
    <T extends Number> T asNumber(Class<T> type);

    /**
     * Convert to string.
     * @return String.
     */
    String asString();

    /**
     * Get bytes itself.
     * @return Bytes.
     */
    byte[] take();
}
