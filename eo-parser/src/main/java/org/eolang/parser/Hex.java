/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

/**
 * Hex encoder for numeric and string literals.
 *
 * <p>Used by {@link LnApplication} and similar emitters to convert
 * parsed literal values into the {@code BB-BB-…} byte-string form the
 * XMIR pipeline expects inside {@code <o base='Φ.bytes'>} elements.</p>
 *
 * <p>Three constructions are supported:</p>
 *
 * <ul>
 *   <li>From a {@code double} — produces 8-byte IEEE-754 big-endian.</li>
 *   <li>From a UTF-8 string — produces the variable-length UTF-8 byte
 *   sequence.</li>
 *   <li>From a raw byte array — direct encoding.</li>
 * </ul>
 *
 * @since 0.1
 */
final class Hex {

    /**
     * The raw byte sequence.
     */
    private final byte[] bytes;

    /**
     * Ctor from a {@code double}. Produces 8-byte IEEE-754 big-endian.
     * @param value Numeric value
     * @checkstyle ConstructorsCodeFreeCheck (3 lines)
     */
    Hex(final double value) {
        this(ByteBuffer.allocate(Double.BYTES).putDouble(value).array());
    }

    /**
     * Ctor from a UTF-8 string. Produces the variable-length UTF-8 byte
     * sequence.
     * @param text The text
     * @checkstyle ConstructorsCodeFreeCheck (3 lines)
     */
    Hex(final String text) {
        this(text.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Primary ctor from a raw byte array.
     * @param raw Bytes
     */
    Hex(final byte[] raw) {
        this.bytes = raw.clone();
    }

    /**
     * Render the bytes as the {@code BB-BB-…} hex string used inside
     * {@code <o base='Φ.bytes'>}.
     *
     * <p>Empty input renders as {@code --}. A single-byte input renders
     * as {@code BB-} (trailing dash). Multi-byte input renders as
     * {@code BB-BB-…} with no trailing dash. The form matches the
     * grammar's {@code BYTES} token (§3.13).</p>
     *
     * @return Hex string
     */
    String asString() {
        final String result;
        if (this.bytes.length == 0) {
            result = "--";
        } else if (this.bytes.length == 1) {
            result = String.format("%02X-", this.bytes[0]);
        } else {
            final StringBuilder out = new StringBuilder(this.bytes.length * 3);
            for (int idx = 0; idx < this.bytes.length; idx = idx + 1) {
                if (idx > 0) {
                    out.append('-');
                }
                out.append(String.format("%02X", this.bytes[idx]));
            }
            result = out.toString();
        }
        return result;
    }
}
