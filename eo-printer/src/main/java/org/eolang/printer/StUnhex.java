/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StEnvelope;
import com.yegor256.xsline.StSequence;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
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
            xnav -> xnav.node().setTextContent(
                String.format(
                    "\"%s\"",
                    StUnhex.escape(
                        new String(
                            StUnhex.buffer(
                                StUnhex.undash(xnav.element("o").text().orElse(""))
                            ).array(),
                            StandardCharsets.UTF_8
                        )
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
     * Prints as int if fractional part of number is 0.
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
                str = BigDecimal.valueOf(num).toBigInteger().toString();
            }
        } else {
            str = Double.toString(num);
        }
        return str;
    }

    /**
     * Escape a decoded string for printing as an EO string literal.
     *
     * <p>Unlike {@code StringEscapeUtils.escapeJava}, which escapes the
     * entire non-ASCII range to {@code \\uNNNN}, this escaper leaves
     * printable Unicode glyphs intact (EO sources are UTF-8). Only the
     * backslash, the double quote and non-printable/control characters
     * are escaped; the latter fall back to {@code \\uNNNN}.</p>
     * @param txt The decoded text
     * @return The escaped text, ready to sit between the quotes
     */
    private static String escape(final String txt) {
        final StringBuilder out = new StringBuilder(txt.length());
        int idx = 0;
        while (idx < txt.length()) {
            final int cpnt = txt.codePointAt(idx);
            out.append(StUnhex.glyph(cpnt));
            idx += Character.charCount(cpnt);
        }
        return out.toString();
    }

    /**
     * Escape a single code point for an EO string literal.
     * @param cpnt The code point
     * @return The escaped representation
     */
    private static String glyph(final int cpnt) {
        final String escaped;
        if (cpnt == '\\') {
            escaped = "\\\\";
        } else if (cpnt == '"') {
            escaped = "\\\"";
        } else if (cpnt == '\n') {
            escaped = "\\n";
        } else if (cpnt == '\t') {
            escaped = "\\t";
        } else if (cpnt == '\r') {
            escaped = "\\r";
        } else if (cpnt == '\b') {
            escaped = "\\b";
        } else if (cpnt == '\f') {
            escaped = "\\f";
        } else if (StUnhex.printable(cpnt)) {
            escaped = new String(Character.toChars(cpnt));
        } else {
            final StringBuilder unicode = new StringBuilder(6);
            for (final char unit : Character.toChars(cpnt)) {
                unicode.append(String.format("\\u%04X", (int) unit));
            }
            escaped = unicode.toString();
        }
        return escaped;
    }

    /**
     * Whether the code point can be rendered as a glyph inside a UTF-8
     * EO source, rather than needing a {@code \\uNNNN} escape.
     * @param cpnt The code point
     * @return TRUE if it is safe to print verbatim
     */
    private static boolean printable(final int cpnt) {
        final boolean safe;
        if (Character.isISOControl(cpnt) || !Character.isDefined(cpnt)) {
            safe = false;
        } else {
            final int type = Character.getType(cpnt);
            safe = type != Character.CONTROL
                && type != Character.FORMAT
                && type != Character.SURROGATE
                && type != Character.PRIVATE_USE
                && type != Character.UNASSIGNED;
        }
        return safe;
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
