/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import java.util.Locale;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * This {@link Shift} turns hex data inside XMIR
 * into EO-printable data.
 *
 * @since 0.29.0
 */
public final class StUnhex implements Shift {

    @Override
    public String uid() {
        return this.getClass().getSimpleName();
    }

    @Override
    public XML apply(final int position, final XML xml) {
        final Directives dirs = new Directives();
        for (final String hex : StUnhex.matches(xml, "float")) {
            final double num = Double.longBitsToDouble(
                Long.parseLong(StUnhex.unspace(hex), 16)
            );
            dirs.xpath(
                String.format(
                    "//o[@data='bytes' and @base='float' and text()='%s']",
                    hex
                )
            ).set(Double.toString(num)).attr("data", "float");
        }
        for (final String hex : StUnhex.matches(xml, "int")) {
            final long num = Long.parseLong(StUnhex.unspace(hex), 16);
            dirs.xpath(
                String.format(
                    "//o[@data='bytes' and @base='int' and text()='%s']",
                    hex
                )
            ).set(Long.toString(num)).attr("data", "int");
        }
        for (final String hex : StUnhex.matches(xml, "bool")) {
            final boolean val = !"00".equals(StUnhex.unspace(hex));
            dirs.xpath(
                String.format(
                    "//o[@data='bytes' and @base='bool' and text()='%s']",
                    hex
                )
            ).set(Boolean.toString(val).toUpperCase(Locale.ENGLISH)).attr("data", "bool");
        }
        for (final String hex : StUnhex.matches(xml, "string")) {
            final String[] parts = StUnhex.unspace(hex).split("(?<=\\G.{2})");
            final ByteBuffer buffer = ByteBuffer.allocate(parts.length);
            for (final String pair : parts) {
                buffer.put((byte) Integer.parseInt(pair, 16));
            }
            final String txt = new String(buffer.array(), StandardCharsets.UTF_8);
            dirs.xpath(
                String.format(
                    "//o[@data='bytes' and @base='string' and text()='%s']",
                    hex
                )
            ).set(txt).attr("data", "string");
        }
        return new XMLDocument(
            new Xembler(dirs).applyQuietly(xml.node())
        );
    }

    /**
     * Take all texts by the given type.
     * @param xml The XML
     * @param type The type
     * @return List of values
     */
    private static Iterable<String> matches(final XML xml, final String type) {
        return new HashSet<>(
            xml.xpath(
                String.format(
                    "//o[@data='bytes' and @base='%s']/text()",
                    type
                )
            )
        );
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
}
