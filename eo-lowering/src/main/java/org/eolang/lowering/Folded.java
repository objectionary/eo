/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.List;

/**
 * A primitive operation on literals alone, computed by the engine itself.
 *
 * <p>The result is answered as data, in the carrier of the operation, so
 * that a body called on literals alone folds to a literal instead of
 * hiding behind a symbol. An operation the engine does not know how to
 * compute faithfully folds to nothing, and the fire mints a row for it
 * instead.</p>
 *
 * @since 0.76.0
 */
public final class Folded {

    /**
     * The operation.
     */
    private final Op operation;

    /**
     * The keys of the operands, the receiver first.
     */
    private final List<String> keys;

    /**
     * Ctor.
     *
     * @param lambda The operation
     * @param operands The keys of the operands, the receiver first
     */
    public Folded(final Op lambda, final List<String> operands) {
        this.operation = lambda;
        this.keys = operands;
    }

    /**
     * The φ-expression of the result.
     *
     * @return The text, or an empty string when the operation is not folded
     */
    public String phi() {
        final String method = this.operation.method();
        final String out;
        if ("number".equals(this.operation.carrier())) {
            out = this.arithmetic(method);
        } else if ("bytes".equals(this.operation.carrier())) {
            out = this.bitwise(method);
        } else {
            out = "";
        }
        return out;
    }

    /**
     * Fold an operation on numbers.
     *
     * @param method The method
     * @return The text, or an empty string
     */
    private String arithmetic(final String method) {
        final double left = this.hex(0).number();
        final String out;
        if ("plus".equals(method)) {
            out = Folded.number(left + this.hex(1).number());
        } else if ("times".equals(method)) {
            out = Folded.number(left * this.hex(1).number());
        } else if ("div".equals(method)) {
            out = Folded.number(left / this.hex(1).number());
        } else if ("gt".equals(method)) {
            out = new Marker(
                String.format("bool:%s", new Hex(left > this.hex(1).number()).text()), "bool"
            ).phi();
        } else {
            out = "";
        }
        return out;
    }

    /**
     * Fold an operation on bytes.
     *
     * @param method The method
     * @return The text, or an empty string
     */
    private String bitwise(final String method) {
        final byte[] left = this.hex(0).bytes();
        final String out;
        if ("and".equals(method) || "or".equals(method)) {
            final byte[] right = this.hex(1).bytes();
            final byte[] result = new byte[Math.max(left.length, right.length)];
            for (int idx = 0; idx < result.length; ++idx) {
                result[idx] = Folded.combined(method, Folded.at(left, idx), Folded.at(right, idx));
            }
            out = Folded.bytes(result);
        } else if ("not".equals(method)) {
            final byte[] result = new byte[left.length];
            for (int idx = 0; idx < result.length; ++idx) {
                result[idx] = (byte) ~left[idx];
            }
            out = Folded.bytes(result);
        } else if ("concat".equals(method)) {
            final byte[] right = this.hex(1).bytes();
            out = Folded.bytes(
                ByteBuffer.allocate(left.length + right.length).put(left).put(right).array()
            );
        } else if ("eq".equals(method)) {
            out = new Marker(
                String.format(
                    "bool:%s", new Hex(Arrays.equals(left, this.hex(1).bytes())).text()
                ),
                "bool"
            ).phi();
        } else if ("size".equals(method)) {
            out = Folded.number(left.length);
        } else {
            out = "";
        }
        return out;
    }

    /**
     * One operand as bytes.
     *
     * @param index The position of the operand
     * @return The bytes
     */
    private Hex hex(final int index) {
        final String key = this.keys.get(index);
        return new Hex(key.substring(key.indexOf(':') + 1));
    }

    /**
     * A byte of an array, zero past its end.
     *
     * @param bytes The array
     * @param idx The position
     * @return The byte
     */
    private static byte at(final byte[] bytes, final int idx) {
        final byte out;
        if (idx < bytes.length) {
            out = bytes[idx];
        } else {
            out = 0;
        }
        return out;
    }

    /**
     * Combine two bytes.
     *
     * @param method The method, {@code and} or {@code or}
     * @param left The left byte
     * @param right The right byte
     * @return The result
     */
    private static byte combined(final String method, final byte left, final byte right) {
        final byte out;
        if ("and".equals(method)) {
            out = (byte) (left & right);
        } else {
            out = (byte) (left | right);
        }
        return out;
    }

    /**
     * A number as data.
     *
     * @param value The number
     * @return The text
     */
    private static String number(final double value) {
        return new Marker(String.format("number:%s", new Hex(value).text()), "number").phi();
    }

    /**
     * Bytes as data.
     *
     * @param value The bytes
     * @return The text
     */
    private static String bytes(final byte[] value) {
        return new Marker(String.format("bytes:%s", new Hex(value).text()), "bytes").phi();
    }
}
