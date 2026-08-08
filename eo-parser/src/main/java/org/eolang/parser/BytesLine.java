/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang.parser;

/**
 * Low-level predicates over the {@code BYTES} hex-run format, shared by
 * the {@link Eo} classifier and its multi-line {@code BYTES} merging.
 * @since 0.57.0
 */
final class BytesLine {

    /**
     * Utility class.
     */
    private BytesLine() {
    }

    /**
     * Whether the body is purely a sequence of hex bytes with dash
     * separators — {@code HH-HH} or {@code HH-HH-} etc.
     * @param body The line body
     * @return True if the body matches the bytes-only pattern
     */
    static boolean isBytesOnly(final String body) {
        boolean valid = !body.isEmpty();
        int idx = 0;
        while (valid && idx < body.length()) {
            if (idx + 1 >= body.length()
                || !BytesLine.hex(body.charAt(idx))
                || !BytesLine.hex(body.charAt(idx + 1))) {
                valid = false;
            } else {
                idx = idx + 2;
                if (idx < body.length() && body.charAt(idx) != '-') {
                    valid = false;
                } else {
                    idx = idx + 1;
                }
            }
        }
        return valid;
    }

    /**
     * Whether a character is a valid BYTES hex digit. Per the grammar
     * ({@code BYTE : [0-9A-F][0-9A-F]}), BYTES accept only uppercase
     * hex — lowercase letters belong to {@code NAME} tokens.
     * @param glyph The character
     * @return True if 0-9 or A-F
     */
    static boolean hex(final char glyph) {
        return glyph >= '0' && glyph <= '9' || glyph >= 'A' && glyph <= 'F';
    }
}
