/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

/**
 * Decoding helpers for byte, octal and unicode escapes inside string
 * and text-block literals.
 *
 * <p>These recipes turn a literal body (without its surrounding quotes)
 * into the raw UTF-8 byte sequence the {@code Φ.bytes} carrier holds
 * (§9.4.2). They are pulled out of {@link Emissions} so that file stays
 * a thin facade over the literal-rendering recipes.</p>
 *
 * @since 0.1
 */
final class ByteEscapes {

    /**
     * Maximum value of a {@code \NNN} octal byte escape (0o377, one byte).
     */
    private static final int MAX_OCTAL_BYTE = 0xFF;

    /**
     * No instances.
     */
    private ByteEscapes() {
    }

    /**
     * Decode a string body to its raw byte representation.
     * @param inner Source body without surrounding quotes
     * @return Decoded bytes
     */
    static byte[] unescapeRawBytes(final String inner) {
        final ByteArrayOutputStream out = new ByteArrayOutputStream(inner.length());
        final StringBuilder text = new StringBuilder(inner.length());
        int idx = 0;
        while (idx < inner.length()) {
            final char glyph = inner.charAt(idx);
            if (glyph != '\\' || idx + 1 >= inner.length()) {
                text.append(glyph);
                idx = idx + 1;
                continue;
            }
            final char next = inner.charAt(idx + 1);
            if (next == 'u') {
                idx = ByteEscapes.appendUnicode(text, inner, idx + 1);
            } else if (next >= '0' && next <= '7') {
                idx = ByteEscapes.rawOctal(out, text, inner, idx + 1);
            } else {
                text.append(ByteEscapes.singleCharEscape(glyph, next));
                idx = idx + 2;
            }
        }
        ByteEscapes.appendText(out, text);
        return out.toByteArray();
    }

    private static int rawOctal(
        final ByteArrayOutputStream out, final StringBuilder text,
        final String body, final int start
    ) {
        ByteEscapes.appendText(out, text);
        int cursor = start;
        int value = 0;
        while (cursor < body.length() && cursor < start + 3
            && body.charAt(cursor) >= '0' && body.charAt(cursor) <= '7') {
            value = value * 8 + body.charAt(cursor) - '0';
            cursor = cursor + 1;
        }
        if (value > ByteEscapes.MAX_OCTAL_BYTE) {
            throw new NumberFormatException(
                String.format(
                    "octal escape \\%s is out of range: value %d exceeds the 1-byte limit of 0o377 (255)",
                    body.substring(start, cursor), value
                )
            );
        }
        out.write(value);
        return cursor;
    }

    private static void appendText(
        final ByteArrayOutputStream out, final StringBuilder text
    ) {
        ByteEscapes.rejectLoneSurrogates(text);
        final byte[] bytes = text.toString().getBytes(StandardCharsets.UTF_8);
        out.write(bytes, 0, bytes.length);
        text.setLength(0);
    }

    private static int appendUnicode(
        final StringBuilder out, final String body, final int start
    ) {
        int cursor = start;
        while (cursor < body.length() && body.charAt(cursor) == 'u') {
            cursor = cursor + 1;
        }
        boolean valid = cursor + 4 <= body.length();
        for (int idx = cursor; valid && idx < cursor + 4; idx = idx + 1) {
            valid = Character.digit(body.charAt(idx), 16) >= 0;
        }
        if (!valid) {
            throw new NumberFormatException(
                String.format(
                    "unicode escape \\%s is not exactly four hexadecimal digits",
                    body.substring(start, Math.min(body.length(), cursor + 4))
                )
            );
        }
        out.append(
            (char) Integer.parseInt(body.substring(cursor, cursor + 4), 16)
        );
        return cursor + 4;
    }

    private static String singleCharEscape(final char head, final char next) {
        final String decoded;
        if (next == 'n') {
            decoded = String.valueOf((char) 10);
        } else if (next == 't') {
            decoded = String.valueOf((char) 9);
        } else if (next == 'r') {
            decoded = String.valueOf((char) 13);
        } else if (next == 'b') {
            decoded = String.valueOf((char) 8);
        } else if (next == 'f') {
            decoded = String.valueOf((char) 12);
        } else if (next == '"' || next == '\'' || next == '\\') {
            decoded = String.valueOf(next);
        } else {
            throw new NumberFormatException(
                String.format("unrecognised escape sequence '%c%c'", head, next)
            );
        }
        return decoded;
    }

    private static void rejectLoneSurrogates(final CharSequence text) {
        int cursor = 0;
        while (cursor < text.length()) {
            final char glyph = text.charAt(cursor);
            if (Character.isHighSurrogate(glyph)
                && cursor + 1 < text.length()
                && Character.isLowSurrogate(text.charAt(cursor + 1))) {
                cursor = cursor + 2;
                continue;
            }
            if (Character.isSurrogate(glyph)) {
                throw new NumberFormatException(
                    String.format(
                        "unicode escape \\u%04X is a lone surrogate, not a valid standalone codepoint",
                        (int) glyph
                    )
                );
            }
            cursor = cursor + 1;
        }
    }
}
