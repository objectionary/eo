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

import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.StEndless;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrFast;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.TrLogged;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.util.logging.Level;

/**
 * Prints XMIR to EO or PHI.
 *
 * <p>This class will help you turn XMIR (XML document) into EOLANG
 * plain text source code or PHI calculus expression. It's as simple as this:</p>
 *
 * <pre> String eo = new Xmir(xml).toEO();</pre>
 * <pre> String phi = new Xmir(xml).toPhi();</pre>
 *
 * <p>Here, the {@code xml} is a {@code String} or an instance
 * of {@code XML} from the jcabi-xml package.</p>
 *
 * @link <a href="https://xml.jcabi.com">xml.jcabi.com</a>
 * @since 0.35.0
 */
public final class Xmir {
    /**
     * Train of transformations that prepare XMIR for conversion to EO.
     */
    private static final Train<Shift> FOR_EO = new TrStepped(
        new TrFast(
            new TrLogged(
                new TrDefault<>(
                    new StEndless(
                        new StClasspath("/org/eolang/parser/print/tuples-to-stars.xsl")
                    ),
                    new StClasspath("/org/eolang/parser/explicit-data.xsl"),
                    new StClasspath("/org/eolang/parser/print/dataized-to-const.xsl"),
                    new StUnhex(),
                    new StClasspath("/org/eolang/parser/print/wrap-data.xsl")
                ),
                Xmir.class,
                Level.FINEST
            ),
            Xmir.class,
            500L
        )
    );

    /**
     * Train of transformations that prepare XMIR for conversion to PHI.
     */
    private static final Train<Shift> FOR_PHI = new TrStepped(
        new TrFast(
            new TrLogged(
                new TrClasspath<>(
                    "/org/eolang/parser/add-refs.xsl",
                    "/org/eolang/parser/expand-qqs.xsl",
                    "/org/eolang/parser/vars-float-up.xsl",
                    "/org/eolang/parser/add-refs.xsl",
                    "/org/eolang/parser/expand-aliases.xsl",
                    "/org/eolang/parser/resolve-aliases.xsl",
                    "/org/eolang/parser/add-refs.xsl",
                    "/org/eolang/parser/add-default-package.xsl",
                    "/org/eolang/parser/explicit-data.xsl",
                    "/org/eolang/parser/phi/incorrect-inners.xsl",
                    "/org/eolang/parser/phi/wrap-default-package.xsl"
                ).back(),
                Xmir.class,
                Level.FINEST
            ),
            Xmir.class,
            500L
        )
    );

    /**
     * The XML.
     */
    private final XML xml;

    /**
     * Ctor.
     * @param src The source
     */
    public Xmir(final XML src) {
        this.xml = src;
    }

    /**
     * Converts XMIR to EO.
     * @return EO representation as {@code String}
     */
    public String toEO() {
        return this.converted(
            Xmir.FOR_EO, "/org/eolang/parser/print/to-eo.xsl", "eo/text()"
        );
    }

    /**
     * Converts XMIR to EO, in reverse notation.
     * @return EO representation as {@code String}
     */
    public String toReversedEO() {
        return this.converted(
            Xmir.FOR_EO, "/org/eolang/parser/print/to-eo-reversed.xsl", "eo/text()"
        );
    }

    /**
     * Converts XMIR to PHI.
     * @return EO representation as {@code String}
     */
    public String toPhi() {
        return this.converted(
            new TrJoined<>(
                Xmir.FOR_PHI,
                new TrDefault<>(
                    new StUnhex(), new StClasspath("/org/eolang/parser/phi/to-phi.xsl")
                )
            ),
            "program/phi/text()"
        );
    }

    /**
     * Converts XMIR to PHI without any syntax sugar.
     * @return EO representation as {@code String}
     */
    public String toPhiNoSugar() {
        return this.converted(
            Xmir.FOR_PHI, "/org/eolang/parser/phi/to-phi-no-sugar.xsl", "program/phi/text()"
        );
    }

    @Override
    public String toString() {
        return this.xml.toString();
    }

    /**
     * Converts XMIR.
     * @param train Train of transformations that prepares XMIR
     * @param xsl Final XSL transformation
     * @param xpath Xpath to retrieve the final result
     * @return XMIR in other representation as {@code String}.
     */
    private String converted(final Train<Shift> train, final String xsl, final String xpath) {
        return this.converted(new TrJoined<>(train.with(new StClasspath(xsl))), xpath);
    }

    /**
     * Converts XMIR.
     * @param train Train of transformations that prepares XMIR
     * @param xpath Xpath to retrieve the final result
     * @return XMIR in other representation as {@code String}.
     */
    private String converted(final Train<Shift> train, final String xpath) {
        return new Xsline(train).pass(this.xml).xpath(xpath).get(0);
    }
}
