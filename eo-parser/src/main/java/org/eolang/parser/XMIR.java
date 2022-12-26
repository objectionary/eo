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

import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Prints XMIR to EO.
 *
 * This class will help you turn XMIR (XML document) into EOLANG
 * plain text source code. It's as simple as this:
 *
 * <pre> String eo = new XMIR(xml).toEO();</pre>
 *
 * Here, the {@code xml} is a {@code String} or an instance
 * of {@code XML} from the jcabi-xml package.
 *
 * @since 0.5
 * @checkstyle AbbreviationAsWordInNameCheck (500 lines)
 * @link https://xml.jcabi.com
 */
public final class XMIR {

    /**
     * The sheet for transformations.
     */
    private static final XSL SHEET = new XSLDocument(
        XMIR.class.getResourceAsStream("xmir-to-eo.xsl"),
        "xmir-to-eo"
    ).with(new ClasspathSources());

    /**
     * The XML content.
     */
    private final XML xml;

    /**
     * Ctor.
     * @param src The source
     */
    public XMIR(final String src) {
        this(new XMLDocument(src));
    }

    /**
     * Ctor.
     * @param src The source
     */
    public XMIR(final XML src) {
        this.xml = src;
    }

    /**
     * Print it to EO.
     *
     * @return The program in EO
     */
    public String toEO() {
        final Directives dirs = new Directives();
        final Iterable<String> floats = new HashSet<>(
            this.xml.xpath(
                "//o[@data='bytes' and @base='float']/text()"
            )
        );
        for (final String hex : floats) {
            final double num = Double.longBitsToDouble(
                Long.parseLong(hex.replace(" ", ""), 16)
            );
            dirs.xpath(
                String.format(
                    "//o[@data='bytes' and @base='float' and text()='%s']",
                    hex
                )
            ).set(Double.toString(num));
        }
        final Iterable<String> strings = new HashSet<>(
            this.xml.xpath(
                "//o[@data='bytes' and @base='string']/text()"
            )
        );
        for (final String hex : strings) {
            final String[] parts = hex.replace(" ", "").split("(?<=\\G.{2})");
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
            ).set(txt);
        }
        return XMIR.SHEET.applyTo(
            new XMLDocument(
                new Xembler(dirs).applyQuietly(this.xml.node())
            )
        );
    }

}
