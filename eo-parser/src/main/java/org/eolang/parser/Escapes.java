/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

/**
 * The bytes a string literal stands for, with its escapes read out.
 *
 * <p>Between the quotes a source file may write a byte in four ways — as
 * itself, as {@code \n} and its kin, as a unicode escape of four hexadecimal
 * digits, or as the octal {@code \101} — and every one of them has to come out
 * as the same bytes the program will hold at runtime. That reading is a
 * subject of its own, sharing nothing with the emission of XMIR around it, so
 * it lives on its own.</p>
 *
 * <p>A text escape and a byte escape do not go into the same place. A unicode
 * escape is a character and becomes whatever UTF-8 makes of it, while a
 * {@code \101} is one byte and goes in as it stands, so the text gathers in a
 * builder and is flushed into the stream whenever an octal escape interrupts
 * it.</p>
 *
 * @since 0.1
 */
final class Escapes {

    /**
     * No instances.
     */
    private Escapes() {
    }

    /**
     * Decode a string body to its raw byte representation.
     * @param inner Source body without surrounding quotes
     * @return Decoded bytes
     */
    static byte[] bytes(final String inner) {
        final ByteArrayOutputStream out = new ByteArrayOutputStream(inner.length());
        final StringBuilder text = new StringBuilder(inner.length());
        int idx = 0;
        while (idx < inner.length()) {
            final char glyph = inner.charAt(idx);
            if (glyph != '\\') {
                text.append(glyph);
                idx = idx + 1;
                continue;
            }
            if (idx + 1 >= inner.length()) {
                throw new NumberFormatException(
                    "backslash at the end of the text has nothing to escape"
                );
            }
            final char next = inner.charAt(idx + 1);
            if (next == 'u') {
                idx = Escapes.appendUnicode(text, inner, idx + 1);
            } else if (next >= '0' && next <= '7') {
                idx = Escapes.rawOctal(out, text, inner, idx + 1);
            } else {
                text.append(Escapes.singleCharEscape(glyph, next));
                idx = idx + 2;
            }
        }
        Escapes.appendText(out, text);
        return out.toByteArray();
    }

    private static int rawOctal(
        final ByteArrayOutputStream out, final StringBuilder text,
        final String body, final int start
    ) {
        Escapes.appendText(out, text);
        int cursor = start;
        int value = 0;
        while (cursor < body.length() && cursor < start + 3
            && body.charAt(cursor) >= '0' && body.charAt(cursor) <= '7') {
            value = value * 8 + body.charAt(cursor) - '0';
            cursor = cursor + 1;
        }
        if (value > 0xFF) {
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
        Escapes.rejectLoneSurrogates(text);
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
