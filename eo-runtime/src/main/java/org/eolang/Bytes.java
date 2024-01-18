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

import EOorg.EOeolang.EObytes;

/**
 * Bytes.
 *
 * Represents byte array of arbitrary size,
 * convertible to a numeric value.
 * Original size is preserved by and, or and xor.
 *
 * @since 1.0
 */
@Versionized
public interface Bytes extends Data<byte[]> {
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
     * @checkstyle MethodNameCheck (2 lines)
     */
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
     * Convert to number.
     * @param type Type.
     * @return Number.
     * @param <T> Numeric type.
     */
    <T extends Number> T asNumber(Class<T> type);

    /**
     * Convert Bytes to Phi.
     * @param bytes Bytes.
     * @return Phi object.
     */
    static Phi toPhi(final Bytes bytes) {
        final Phi object = new EObytes(Phi.Φ);
        object.attr("Δ").put(new Data.Value<>(bytes.take()));
        return new PhConst(object);
    }

}
