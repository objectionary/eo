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
package org.eolang.parser.xmir;

import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import org.eolang.parser.StUnhex;

/**
 * Prints XMIR to EO.
 * @since 0.35.0
 * @checkstyle JavadocStyleCheck (200 lines)
 */
public interface Xmir {
    /**
     * Converts XMIR to EO.
     * @return EO representation as {@code String}
     */
    String toEO();

    /**
     * Default Xmir that prints EO with straight vertical methods.
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
         * Train of transformations.
         */
        private static final Train<Shift> TRAIN = new TrDefault<>(
            new StClasspath("/org/eolang/parser/explicit-data.xsl"),
            new StUnhex(),
            new StClasspath("/org/eolang/parser/wrap-method-calls.xsl")
        );

        /**
         * Default xmir-to-eo XSL transformation.
         */
        private static final String TO_EO = "/org/eolang/parser/xmir-to-eo.xsl";

        /**
         * The XML.
         */
        private final XML xml;

        /**
         * Result to-EO transformation.
         */
        private final String xsl;

        /**
         * Ctor.
         * @param src The source
         */
        public Default(final XML src) {
            this(src, Xmir.Default.TO_EO);
        }

        /**
         * Ctor.
         * @param src The source
         * @param classpath To-EO transformation classpath
         */
        public Default(final XML src, final String classpath) {
            this.xml = src;
            this.xsl = classpath;
        }

        @Override
        public String toEO() {
            return new Xsline(
                Xmir.Default.TRAIN.with(
                    new StClasspath(this.xsl)
                )
            )
                .pass(this.xml)
                .xpath("eo/text()")
                .get(0);
        }
    }

    /**
     * Simplified xmir that allows printing of small parts EO.
     * <p>
     * This printer is important in cases when you need to print only some parts of EO
     * without having the whole EO document.
     * </p>
     * <p>
     * For example, instead of generating the full structure of EO document like the following
     * {@code
     * <program>
     *    <objects>
     *        <o base=".plus">
     *           <o base="int" data="bytes">00 00 00 00 00 00 00 01</o>
     *           <o base="int" data="bytes">00 00 00 00 00 00 00 02</o>
     *        </o>
     *    </objects>
     * </program>
     * }
     * You can generate only the part of the EO document that you need like the following
     * {@code
     * <o base=".plus">
     *     <o base="int" data="bytes">00 00 00 00 00 00 00 01</o>
     *     <o base="int" data="bytes">00 00 00 00 00 00 00 02</o>
     * </o>
     * }
     * </p>
     * It significantly simplifies the process of printing EO, especially in tests.
     * You can read more about the problem right
     * <a href="https://github.com/objectionary/eo/issues/3343">here</a>
     * @since 0.40
     */
    final class Simplified implements Xmir {

        /**
         * Train of transformations.
         */
        private static final Train<Shift> TRAIN = new TrDefault<>(
            new StClasspath("/org/eolang/parser/explicit-data.xsl"),
            new StUnhex(),
            new StClasspath("/org/eolang/parser/wrap-method-calls.xsl")
        );

        /**
         * Default xmir-to-eo-simplified XSL transformation.
         */
        private static final String TO_EO = "/org/eolang/parser/xmir-to-eo-simplified.xsl";

        /**
         * The XML to transform.
         */
        private final XML xml;

        /**
         * Transformation.
         */
        private final String xsl;

        /**
         * Constructor.
         * @param src The source to transform.
         */
        public Simplified(final XML src) {
            this(src, Xmir.Simplified.TO_EO);
        }

        /**
         * Constructor.
         * @param src The source to transform.
         * @param classpath Transformation classpath.
         */
        public Simplified(final XML src, final String classpath) {
            this.xml = src;
            this.xsl = classpath;
        }

        @Override
        public String toEO() {
            return new Xsline(Simplified.TRAIN.with(new StClasspath(this.xsl)))
                .pass(this.xml).xpath("eo/text()").get(0);
        }
    }
}
