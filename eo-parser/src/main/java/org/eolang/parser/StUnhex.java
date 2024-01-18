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
import java.util.Locale;
import org.apache.commons.text.StringEscapeUtils;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * This {@link Shift} turns hex data inside XMIR
 * into EO-printable data.
 *
 * @since 0.29.0
 */
public final class StUnhex extends StEnvelope {

    /**
     * Ctor.
     */
    public StUnhex() {
        super(
            new StEndless(
                new StSequence(
                    StUnhex.class.getSimpleName(),
                    new StXPath(
                        StUnhex.xpath("float"),
                        xml -> StUnhex.append(
                            "float",
                            Double.toString(
                                StUnhex.buffer(
                                    StUnhex.unspace(xml.xpath("./o/text()").get(0))
                                ).getDouble()
                            )
                        )
                    ),
                    new StXPath(
                        StUnhex.xpath("int"),
                        xml -> StUnhex.append(
                            "int",
                            Long.toString(
                                StUnhex.buffer(
                                    StUnhex.unspace(xml.xpath("./o/text()").get(0))
                                ).getLong()
                            )
                        )
                    ),
                    new StXPath(
                        StUnhex.xpath("bool"),
                        xml -> StUnhex.append(
                            "bool",
                            Boolean.toString(
                                !"00".equals(
                                    StUnhex.unspace(xml.xpath("./o/text()").get(0))
                                )
                            ).toUpperCase(Locale.ENGLISH)
                        )
                    ),
                    new StXPath(
                        StUnhex.xpath("string"),
                        xml -> StUnhex.append(
                            "string",
                            StringEscapeUtils.escapeJava(
                                new String(
                                    StUnhex.buffer(
                                        StUnhex.unspace(xml.xpath("./o/text()").get(0))
                                    ).array(),
                                    StandardCharsets.UTF_8
                                )
                            )
                        )
                    ),
                    new StXPath(
                        "(//o[@data='bytes' and (@base='bytes' or @base='org.eolang.bytes') and empty(text())]/parent::o[(@base='string' or @base='org.eolang.string')])[1]",
                        xml -> new Directives().set("").attr("data", "string")
                    )
                )
            )
        );
    }

    /**
     * Make a byte buffer from a string.
     * @param txt The text
     * @return The buffer of bytes
     */
    private static ByteBuffer buffer(final String txt) {
        final String[] parts = txt.split("(?<=\\G.{2})");
        final ByteBuffer buffer = ByteBuffer.allocate(parts.length);
        for (final String pair : parts) {
            buffer.put((byte) Integer.parseInt(pair, 16));
        }
        buffer.position(0);
        return buffer;
    }

    /**
     * Remove all spaces from the given text, like "0A 7E 43" to "0A7E43".
     * @param txt The text
     * @return The same text, but without spaces
     */
    private static String unspace(final String txt) {
        final StringBuilder out = new StringBuilder(txt.length());
        for (final char chr : txt.toCharArray()) {
            if (chr == ' ') {
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
            "(//o[@data='bytes' and (@base='bytes' or @base='org.eolang.bytes') and not(empty(text()))]/parent::o[(@base='%s' or @base='org.eolang.%1$s')])[1]",
            type
        );
    }

    /**
     * Append Xemply instructions.
     * @param type The type to match
     * @param after Value after
     * @return Dirs
     */
    private static Iterable<Directive> append(final String type,
        final String after) {
        return new Directives().set(after).attr("data", type);
    }
}

