/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StEnvelope;
import com.yegor256.xsline.StSequence;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.Optional;
import org.eolang.parser.StXnav;

/**
 * This {@link Shift} turns hex data inside XMIR.
 * into EO-printable data.
 * @since 0.29.0
 */
final class StUnhex extends StEnvelope {

    /**
     * Xpath for finding bytes.
     */
    private static final String BYTES =
        "//o[@base='Φ.bytes' and o[1][string-length(normalize-space(text()))>0]]";

    /**
     * Unexing via {@link com.github.lombrozo.xnav.Xnav}.
     */
    static final Shift XNAV = new StSequence(
        StUnhex.class.getSimpleName(),
        new StXnav(
            StUnhex.BYTES,
            xnav -> new Payload(xnav).replace(
                xnav.element("o").text().orElse("")
            )
        ),
        new StXnav(
            StUnhex.elements("number"),
            xnav -> StUnhex.buffer(
                StUnhex.undash(xnav.element("o").text().orElse(""))
            ).ifPresent(
                buffer -> {
                    if (buffer.remaining() == Double.BYTES) {
                        final double number = buffer.getDouble();
                        if (!Double.isNaN(number) && !Double.isInfinite(number)) {
                            new Payload(xnav).replace(StUnhex.number(number));
                        }
                    }
                }
            )
        ),
        new StXnav(
            StUnhex.elements("string"),
            xnav -> StUnhex.buffer(
                StUnhex.undash(xnav.element("o").text().orElse(""))
            ).ifPresent(
                buffer -> StUnhex.decode(buffer.array()).ifPresent(
                    decoded -> new Payload(xnav).replace(
                        String.format("\"%s\"", StUnhex.escape(decoded))
                    )
                )
            )
        )
    );

    /**
     * Ctor.
     */
    StUnhex() {
        this(StUnhex.XNAV);
    }

    /**
     * Base ctor.
     * @param origin Original shift
     */
    StUnhex(final Shift origin) {
        super(origin);
    }

    /**
     * Convert given number to string.
     * Prints as int when the fractional part is zero and the magnitude fits in long.
     * @param num Number to convert
     * @return Number converted to string
     */
    private static String number(final Double num) {
        final String str;
        if (num % 1 == 0) {
            if ("-0.0".equals(num.toString())) {
                str = "-0";
            } else if (Math.abs(num) < 0x1p63) {
                str = Long.toString(num.longValue());
            } else {
                str = Double.toString(num).replace('E', 'e');
            }
        } else {
            str = Double.toString(num);
        }
        return str;
    }

    /**
     * Make a byte buffer from a hex string.
     *
     * <p>Each byte is two hex characters, so a payload with an odd number of
     * digits has a stray trailing nibble that cannot form a complete byte.
     * Rather than reading past the end of the string and throwing, this
     * method returns {@link Optional#empty()} for such a payload, so that
     * callers leave the offending {@code Φ.number}/{@code Φ.string} node
     * untouched - the same strategy the number branch already uses for a
     * wrong byte count.</p>
     *
     * @param txt The text
     * @return The buffer of bytes, or empty if the payload has odd length
     */
    private static Optional<ByteBuffer> buffer(final String txt) {
        final int len = txt.length();
        final Optional<ByteBuffer> result;
        if (len % 2 == 0) {
            final ByteBuffer buffer = ByteBuffer.allocate(len / 2);
            for (int idx = 0; idx < len; idx += 2) {
                buffer.put((byte) Integer.parseInt(txt.substring(idx, idx + 2), 16));
            }
            buffer.position(0);
            result = Optional.of(buffer);
        } else {
            result = Optional.empty();
        }
        return result;
    }

    /**
     * Strictly decode UTF-8 bytes into a string.
     * The lenient JDK decoder ({@code new String(bytes, UTF_8)}) silently
     * replaces malformed or incomplete sequences with {@code U+FFFD}, which
     * does not round-trip: re-parsing the printed literal yields the bytes
     * {@code EF-BF-BD} instead of the original ones, so a pure formatting pass
     * would change the program. A strict decoder reports such input instead,
     * and this method returns {@link Optional#empty()} for it, leaving the
     * caller to keep the explicit {@code Φ.string}/{@code Φ.bytes} structure.
     * @param bytes The raw bytes
     * @return The decoded string, or empty if the bytes are not valid UTF-8
     */
    private static Optional<String> decode(final byte[] bytes) {
        Optional<String> result;
        try {
            result = Optional.of(
                StandardCharsets.UTF_8.newDecoder()
                    .onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT)
                    .decode(ByteBuffer.wrap(bytes))
                    .toString()
            );
        } catch (final CharacterCodingException ex) {
            result = Optional.empty();
        }
        return result;
    }

    /**
     * Escape a decoded string for an EO string literal. Only the
     * characters an EO literal actually requires are escaped — the
     * backslash, the double quote, and the control characters (as
     * {@code \n}, {@code \t}, {@code \r}, {@code \b}, {@code \f}, or a
     * {@code \\uXXXX} fallback). Printable Unicode glyphs are left
     * intact, since EO sources are UTF-8.
     * @param txt The decoded text
     * @return The escaped text, ready to sit between double quotes
     */
    private static String escape(final String txt) {
        final StringBuilder out = new StringBuilder(txt.length());
        int idx = 0;
        while (idx < txt.length()) {
            final int point = txt.codePointAt(idx);
            out.append(StUnhex.escaped(point));
            idx += Character.charCount(point);
        }
        return out.toString();
    }

    /**
     * Escape a single code point for an EO string literal.
     * @param point The code point
     * @return The verbatim glyph, or its escape sequence
     */
    private static String escaped(final int point) {
        final String result;
        if (point == '\\') {
            result = "\\\\";
        } else if (point == '"') {
            result = "\\\"";
        } else if (point == '\n') {
            result = "\\n";
        } else if (point == '\t') {
            result = "\\t";
        } else if (point == '\r') {
            result = "\\r";
        } else if (point == '\b') {
            result = "\\b";
        } else if (point == '\f') {
            result = "\\f";
        } else if (Character.isISOControl(point)) {
            result = String.format("\\u%04x", point);
        } else {
            result = new String(Character.toChars(point));
        }
        return result;
    }

    /**
     * Remove all dashes from the given text, like "0A-7E-43" to "0A7E43".
     * @param txt The text
     * @return The same text, but without dashes
     */
    private static String undash(final String txt) {
        final StringBuilder out = new StringBuilder(txt.length());
        for (int idx = 0; idx < txt.length(); ++idx) {
            final char chr = txt.charAt(idx);
            if (chr == '-') {
                continue;
            }
            out.append(chr);
        }
        return out.toString();
    }

    /**
     * Find elements by XPath for the given type.
     * @param type The type to match
     * @return XPath
     */
    private static String elements(final String type) {
        return String.format(
            "//o[@base='Φ.%s' and o[1][@base='Φ.bytes' and string-length(normalize-space(text()))>0]]",
            type
        );
    }
}
