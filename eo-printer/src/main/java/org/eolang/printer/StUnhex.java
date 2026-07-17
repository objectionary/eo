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
import java.util.regex.Pattern;
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
     * Regex for splitting hex pairs.
     */
    private static final Pattern HEX_PAIR = Pattern.compile("(?<=\\G.{2})");

    /**
     * Unexing via {@link com.github.lombrozo.xnav.Xnav}.
     */
    static final Shift XNAV = new StSequence(
        StUnhex.class.getSimpleName(),
        new StXnav(
            StUnhex.BYTES,
            xnav -> xnav.node().setTextContent(
                xnav.element("o").text().orElse("")
            )
        ),
        new StXnav(
            StUnhex.elements("number"),
            xnav -> {
                final double number = StUnhex.buffer(
                    StUnhex.undash(xnav.element("o").text().orElse(""))
                ).getDouble();
                if (!Double.isNaN(number) && !Double.isInfinite(number)) {
                    xnav.node().setTextContent(StUnhex.number(number));
                }
            }
        ),
        new StXnav(
            StUnhex.elements("string"),
            xnav -> StUnhex.decode(
                StUnhex.buffer(
                    StUnhex.undash(xnav.element("o").text().orElse(""))
                ).array()
            ).ifPresent(
                decoded -> xnav.node().setTextContent(
                    String.format("\"%s\"", StUnhex.escape(decoded))
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
     * Make a byte buffer from a string.
     * @param txt The text
     * @return The buffer of bytes
     */
    private static ByteBuffer buffer(final String txt) {
        final ByteBuffer buffer;
        if (txt.isEmpty()) {
            buffer = ByteBuffer.allocate(0);
        } else {
            final String[] parts = StUnhex.HEX_PAIR.split(txt);
            buffer = ByteBuffer.allocate(parts.length);
            for (final String pair : parts) {
                buffer.put((byte) Integer.parseInt(pair, 16));
            }
            buffer.position(0);
        }
        return buffer;
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
        for (final char chr : txt.toCharArray()) {
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
