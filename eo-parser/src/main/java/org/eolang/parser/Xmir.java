/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.util.Collection;
import java.util.List;
import javax.xml.namespace.NamespaceContext;
import org.w3c.dom.Node;
import org.w3c.dom.ls.LSResourceResolver;
import org.xml.sax.SAXParseException;

/**
 * Prints XMIR to EO.
 *
 * <p>This class will help you turn XMIR (XML document) into EOLANG
 * plain text source code. It's as simple as this:</p>
 *
 * <pre> String eo = new Xmir(xml).toEO();</pre>
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
     * Train of transformations that prepare XMIR for conversion to EO.
     */
    private static final Xsline FOR_EO = new Xsline(
        new TrFull(
            new TrDefault<>(
                new StUnhex(),
                new StClasspath("/org/eolang/parser/print/tuples-to-stars.xsl"),
                new StClasspath("/org/eolang/parser/print/inline-cactoos.xsl"),
                new StClasspath("/org/eolang/parser/print/dataized-to-const.xsl"),
                new StClasspath("/org/eolang/parser/print/unnecessary-as.xsl"),
                new StClasspath("/org/eolang/parser/print/to-eo.xsl")
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
        return this.converted(Xmir.FOR_EO, "eo");
    }

    /**
     * Converts XMIR.
     * @param train Train of transformations that prepares XMIR
     * @param node XML node name
     * @return XMIR in other representation as {@link String}.
     */
    private String converted(final Xsline train, final String node) {
        final XML xmir = train.pass(this.xml);
        Logger.debug(this, "XMIR after converting to %s:\n%s", node, xmir);
        return new Xnav(xmir.inner())
            .element("object")
            .element(node)
            .text()
            .orElseThrow(
                () -> new IllegalStateException(
                    String.format(
                        "Couldn't find element '/object/%s' after converting to %s",
                        node, node
                    )
                )
            );
    }
}
