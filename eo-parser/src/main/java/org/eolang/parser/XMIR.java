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
import com.jcabi.xml.XSLDocument;

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
        return new XSLDocument(
            this.getClass().getResourceAsStream("xmir-to-eo.xsl"),
            "xmir-to-eo"
        ).with(new ClasspathSources()).applyTo(this.xml);
    }

}
