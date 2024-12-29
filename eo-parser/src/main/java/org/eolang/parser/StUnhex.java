/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.parser;

import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StEndless;
import com.yegor256.xsline.StEnvelope;
import com.yegor256.xsline.StSequence;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import org.apache.commons.text.StringEscapeUtils;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * This {@link Shift} turns hex data inside XMIR
 * into EO-printable data.
 *
 * @since 0.29.0
 */
final class StUnhex extends StEnvelope {

    /**
     * Ctor.
     */
    StUnhex() {
        super(
            new StEndless(
                new StSequence(
                    StUnhex.class.getSimpleName(),
                    new StXPath(
                        StUnhex.xpath("number"),
                        xml -> {
                            final double number = StUnhex.buffer(
                                StUnhex.undash(xml.xpath("./o/text()").get(0))
                            ).getDouble();
                            final Iterable<Directive> dirs;
                            if (Double.isNaN(number) || Double.isInfinite(number)) {
                                dirs = new Directives().attr("skip", "");
                            } else {
                                dirs = StUnhex.append(StUnhex.number(number));
                            }
                            return dirs;
                        }
                    ),
                    new StXPath(
                        StUnhex.xpath("string"),
                        xml -> StUnhex.append(
                            String.format(
                                "\"%s\"",
                                StringEscapeUtils.escapeJava(
                                    new String(
                                        StUnhex.buffer(
                                            StUnhex.undash(xml.xpath("./o/text()").get(0))
                                        ).array(),
                                        StandardCharsets.UTF_8
                                    )
                                )
                            )
                        )
                    )
                )
            )
        );
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
     * Make XPath.
     * @param type The type to match
     * @return XPath
     */
    private static String xpath(final String type) {
        return String.format(
            "(//o[(@base='%s' or @base='org.eolang.%1$s') and(not(@skip)) and o[not(o) and string-length(normalize-space(text()))>0 and (@base='bytes' or @base='org.eolang.bytes')]])[1]",
            type
        );
    }

    /**
     * Append Xemply instructions.
     * @param after Value after
     * @return Dirs
     */
    private static Iterable<Directive> append(final String after) {
        return new Directives().set(after);
    }
}

