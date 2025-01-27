/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.StEndless;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.util.Collection;
import java.util.List;
import javax.xml.namespace.NamespaceContext;
import org.w3c.dom.Node;
import org.w3c.dom.ls.LSResourceResolver;
import org.xml.sax.SAXParseException;

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
@SuppressWarnings("PMD.TooManyMethods")
public final class Xmir implements XML {
    /**
     * Unhex transformation.
     */
    private static final Shift UNHEX = new StUnhex();

    /**
     * Train of transformations that prepare XMIR for conversion to EO.
     */
    private static final Train<Shift> FOR_EO = new TrFull(
        new TrDefault<>(
            new StEndless(
                new StClasspath("/org/eolang/parser/print/tuples-to-stars.xsl")
            ),
            new StClasspath("/org/eolang/parser/shake/explicit-data.xsl"),
            new StClasspath("/org/eolang/parser/print/dataized-to-const.xsl"),
            new StUnhex(),
            new StClasspath("/org/eolang/parser/print/wrap-data.xsl")
        )
    );

    /**
     * Train of transformations that prepare XMIR for conversion to PHI.
     */
    private static final Train<Shift> FOR_PHI = new TrFull(
        new TrClasspath<>(
            "/org/eolang/parser/shake/vars-float-up.xsl",
            "/org/eolang/parser/shake/build-fqns.xsl",
            "/org/eolang/parser/shake/expand-qqs.xsl",
            "/org/eolang/parser/shake/expand-aliases.xsl",
            "/org/eolang/parser/shake/resolve-aliases.xsl",
            "/org/eolang/parser/shake/add-default-package.xsl",
            "/org/eolang/parser/shake/explicit-data.xsl",
            "/org/eolang/parser/phi/wrap-default-package.xsl"
        ).back()
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

    @Override
    public String toString() {
        return this.xml.toString();
    }

    @Override
    public List<String> xpath(final String xpath) {
        return this.xml.xpath(xpath);
    }

    @Override
    public List<XML> nodes(final String xpath) {
        return this.xml.nodes(xpath);
    }

    @Override
    public XML registerNs(final String pfx, final Object uri) {
        return this.xml.registerNs(pfx, uri);
    }

    @Override
    public XML merge(final NamespaceContext ctx) {
        return this.xml.merge(ctx);
    }

    @Override
    @Deprecated
    public Node node() {
        throw new UnsupportedOperationException("deprecated");
    }

    @Override
    public Node inner() {
        return this.xml.inner();
    }

    @Override
    public Node deepCopy() {
        return this.xml.deepCopy();
    }

    @Override
    public Collection<SAXParseException> validate(final LSResourceResolver resolver) {
        return this.xml.validate(resolver);
    }

    @Override
    public Collection<SAXParseException> validate(final XML xsd) {
        return this.xml.validate(xsd);
    }

    /**
     * Converts XMIR to EO.
     * @return EO representation as {@link String}
     */
    public String toEO() {
        return this.converted(
            Xmir.FOR_EO, "/org/eolang/parser/print/to-eo.xsl",
            "eo"
        );
    }

    /**
     * Converts XMIR to EO, in reverse notation.
     * @return EO representation as {@link String}
     */
    public String toReversedEO() {
        return this.converted(
            Xmir.FOR_EO, "/org/eolang/parser/print/to-eo-reversed.xsl",
            "eo"
        );
    }

    /**
     * Converts XMIR to PHI.
     * @return PHI representation as {@link String}
     */
    public String toPhi() {
        return this.toPhi(false);
    }

    /**
     * Converts XMIR to PHI.
     * @param conservative Add empty braces to formations or not
     * @return PHI representation as {@link  String}.
     */
    public String toPhi(final boolean conservative) {
        return this.converted(
            new TrJoined<>(
                Xmir.FOR_PHI,
                new TrDefault<>(
                    Xmir.UNHEX,
                    new StClasspath(
                        "/org/eolang/parser/phi/to-phi.xsl",
                        String.format("conservative %b", conservative)
                    )
                )
            ),
            "phi"
        );
    }

    /**
     * Converts XMIR to PHI without any syntax sugar.
     * @return PHI representation as {@link String}
     */
    public String toSaltyPhi() {
        return this.converted(
            Xmir.FOR_PHI, "/org/eolang/parser/phi/to-salty-phi.xsl",
            "phi"
        );
    }

    /**
     * Converts XMIR.
     * @param train Train of transformations that prepares XMIR
     * @param xsl Final XSL transformation
     * @param xpath Xpath to retrieve the final result
     * @return XMIR in other representation as {@link String}.
     */
    private String converted(final Train<Shift> train, final String xsl, final String xpath) {
        return this.converted(train.with(new StClasspath(xsl)), xpath);
    }

    /**
     * Converts XMIR.
     * @param train Train of transformations that prepares XMIR
     * @param node XML node name
     * @return XMIR in other representation as {@link String}.
     */
    private String converted(final Train<Shift> train, final String node) {
        return new Xnav(new Xsline(train).pass(this.xml).inner())
            .element("program")
            .element(node)
            .text()
            .get();
    }
}
