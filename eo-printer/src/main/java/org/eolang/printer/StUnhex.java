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
 *
 * @since 0.29.0
 */
final class StUnhex extends StEnvelope {

    /**
     * Xpath for finding bytes.
     *
     * <p>The {@code not(o)} is what tells a literal from an application of
     * {@code bytes} to one. {@link StXnav} evaluates this through
     * {@link com.github.lombrozo.xnav.Xnav}, where {@code text()} in a
     * predicate reads every descendant's text, not the direct children's,
     * so the wrapper of a literal reads as though it carried the hex
     * itself. A literal's first child holds that text and nothing else.</p>
     */
    private static final String BYTES =
        "//o[@base='Φ.bytes' and o[1][not(o) and string-length(normalize-space(text()))>0]]";

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
     *
     * @param origin Original shift
     */
    StUnhex(final Shift origin) {
        super(origin);
    }

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
            str = Double.toString(num).replace('E', 'e');
        }
        return str;
    }

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

    private static String elements(final String type) {
        return String.format(
            "//o[@base='Φ.%s' and o[1][@base='Φ.bytes' and string-length(normalize-space(text()))>0]]",
            type
        );
    }
}
