/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
package org.eolang.parser.xmir;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.Xsline;
import org.cactoos.Text;
import org.cactoos.scalar.Unchecked;
import org.eolang.parser.StUnhex;

/**
 * Prints XMIR to EO.
 *
 * @since 0.35.0
 */
public interface Xmir {
    /**
     * Converts XMIR to EO.
     * @return EO representation as {@code String}
     */
    String toEO();

    /**
     * Default Xmir that prints EO with strait vertical methods.
     *
     * This class will help you turn XMIR (XML document) into EOLANG
     * plain text source code. It's as simple as this:
     *
     * <pre> String eo = new Xmir.Default(xml).toEO();</pre>
     *
     * Here, the {@code xml} is a {@code String} or an instance
     * of {@code XML} from the jcabi-xml package.
     * @link <a href="https://xml.jcabi.com">xml.jcabi.com</a>
     * @since 0.35.5
     */
    final class Default implements Xmir {
        /**
         * Wrap method calls XSL.
         */
        private static final String WRAP = "/org/eolang/parser/wrap-method-calls.xsl";

        /**
         * Default xmir-to-eo XSL transformation.
         */
        private static final String TO_EO = "/org/eolang/parser/xmir-to-eo.xsl";

        /**
         * The XML content.
         */
        private final Unchecked<String> content;

        /**
         * XSL transformation sheets.
         */
        private final String[] sheets;

        /**
         * Ctor.
         * @param src The source
         */
        public Default(final String src) {
            this(new XMLDocument(src));
        }

        /**
         * Ctor.
         * @param src The source
         */
        public Default(final Text src) {
            this(new Unchecked<>(src::asString), Xmir.Default.TO_EO);
        }

        /**
         * Ctor.
         * @param src The source
         * @param xsl To-EO transformation
         */
        public Default(final Text src, final String xsl) {
            this(new Unchecked<>(src::asString), xsl);
        }

        /**
         * Ctor.
         * @param src The source
         */
        public Default(final XML src, final String xsl) {
            this(new Unchecked<>(src::toString), xsl);
        }

        /**
         * Ctor.
         * @param src The source
         */
        public Default(final XML src) {
            this(new Unchecked<>(src::toString), Xmir.Default.TO_EO);
        }

        /**
         * Ctor.
         * @param src The source
         * @param xsl To-EO transformation
         */
        Default(final Unchecked<String> src, final String xsl) {
            this(src, new String[] { Xmir.Default.WRAP, xsl });
        }

        /**
         * Ctor.
         * @param src The source
         */
        private Default(final Unchecked<String> src, final String[] sheets) {
            this.content = src;
            this.sheets = sheets;
        }

        @Override
        public String toEO() {
            return new Xsline(
                new TrJoined<>(
                    new TrDefault<>(new StUnhex()),
                    new TrClasspath<>(this.sheets).back()
                )
            )
                .pass(new XMLDocument(this.content.value()))
                .xpath("eo/text()")
                .get(0);
        }
    }
}
