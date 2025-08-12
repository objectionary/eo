/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StEnvelope;
import com.yegor256.xsline.StSequence;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import org.apache.commons.text.StringEscapeUtils;
import org.w3c.dom.Node;

/**
 * This {@link Shift} turns hex data inside XMIR.
 * into EO-printable data.
 *
 * @since 0.29.0
 */
final class StUnhex extends StEnvelope {
    /**
     * Xpath for finding bytes.
     */
    private static final String BYTES =
        "//o[@base='Φ.org.eolang.bytes' and o[1][string-length(normalize-space(text()))>0]]";

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
                final Node node = xnav.node();
                if (!Double.isNaN(number) && !Double.isInfinite(number)) {
                    node.setTextContent(StUnhex.number(number));
                }
            }
        ),
        new StXnav(
            StUnhex.elements("string"),
            xnav -> xnav.node().setTextContent(
                String.format(
                    "\"%s\"",
                    StringEscapeUtils.escapeJava(
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
            } else {
                str = Long.toString(num.longValue());
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
            final String[] parts = txt.split("(?<=\\G.{2})");
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
            "//o[@base='Φ.org.eolang.%s' and o[1][@base='Φ.org.eolang.bytes' and string-length(normalize-space(text()))>0]]",
            type
        );
    }
}
