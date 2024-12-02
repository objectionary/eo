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

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StAfter;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.StLambda;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.TrLogged;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;

/**
 * Prints XMIR to EO.
 *
 * <p>Default Xmir that prints EO with straight vertical methods.</p>
 *
 * <p>This class will help you turn XMIR (XML document) into EOLANG
 * plain text source code. It's as simple as this:</p>
 *
 * <pre> String eo = new Xmir.Default(xml).toEO();</pre>
 *
 * <p>Here, the {@code xml} is a {@code String} or an instance
 * of {@code XML} from the jcabi-xml package.</p>
 *
 * @link <a href="https://xml.jcabi.com">xml.jcabi.com</a>
 * @since 0.35.0
 */
public final class Xmir {

    /**
     * Train of transformations.
     */
    private static final Train<Shift> TRAIN = new TrLogged(
        new TrLambda(
            new TrDefault<>(
                new StClasspath("/org/eolang/parser/explicit-data.xsl"),
                new StUnhex(),
                new StClasspath("/org/eolang/parser/wrap-method-calls.xsl"),
                new StClasspath("/org/eolang/parser/wrap-data.xsl")
            ),
            shift -> new StAfter(
                shift,
                new StLambda(
                    shift::uid,
                    (pos, xml) -> {
                        Logger.debug(Xmir.class, "Step #%d\n%s", pos, xml);
                        return xml;
                    }
                )
            )
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
        return this.via("/org/eolang/parser/xmir-to-eo.xsl");
    }

    /**
     * Converts XMIR to EO, in reverse notation.
     * @return EO representation as {@code String}
     */
    public String toReversed() {
        return this.via("/org/eolang/parser/xmir-to-eo-reversed.xsl");
    }

    /**
     * Converts XMIR to EO, via provided XSL.
     * @param xsl The XSL
     * @return EO representation as {@code String}
     */
    private String via(final String xsl) {
        return new Xsline(
            Xmir.TRAIN.with(
                new StClasspath(xsl)
            )
        ).pass(this.xml).xpath("eo/text()").get(0);
    }

}
