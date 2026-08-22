/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang.parser;

/**
 * A single source line body examined as a possible {@code BYTES} hex-run
 * ({@code HH-HH} or {@code HH-HH-} etc.), used by the {@link Eo} classifier
 * and its multi-line {@code BYTES} merging.
 * @since 0.57.0
 */
final class BytesLine {

    /**
     * The line body.
     */
    private final String body;

    /**
     * Ctor.
     * @param line The line body to examine
     */
    BytesLine(final String line) {
        this.body = line;
    }

    /**
     * Whether the body is purely a sequence of hex bytes with dash
     * separators — {@code HH-HH} or {@code HH-HH-} etc. Per the grammar
     * ({@code BYTE : [0-9A-F][0-9A-F]}), only uppercase hex counts here,
     * since lowercase letters belong to {@code NAME} tokens.
     * @return True if the body matches the bytes-only pattern
     */
    boolean onlyBytes() {
        boolean valid = !this.body.isEmpty();
        int idx = 0;
        while (valid && idx < this.body.length()) {
            if (idx + 1 >= this.body.length()
                || !BytesLine.hex(this.body.charAt(idx))
                || !BytesLine.hex(this.body.charAt(idx + 1))) {
                valid = false;
            } else {
                idx = idx + 2;
                if (idx < this.body.length() && this.body.charAt(idx) != '-') {
                    valid = false;
                } else {
                    idx = idx + 1;
                }
            }
        }
        return valid;
    }

    private static boolean hex(final char glyph) {
        return glyph >= '0' && glyph <= '9' || glyph >= 'A' && glyph <= 'F';
    }
}
