/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import javax.xml.namespace.NamespaceContext;
import org.eolang.parser.TrFull;
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
                new StClasspath("/org/eolang/printer/print/restore-local-names.xsl"),
                new StClasspath("/org/eolang/printer/print/tuples-to-stars.xsl"),
                new StClasspath("/org/eolang/printer/print/inline-cactoos.xsl"),
                new StClasspath("/org/eolang/printer/print/dataized-to-const.xsl"),
                new StClasspath("/org/eolang/printer/print/unnecessary-as.xsl"),
                new StClasspath("/org/eolang/printer/print/merge-monikers.xsl"),
                new StClasspath("/org/eolang/printer/print/renest-pipe-monikers.xsl"),
                new StClasspath("/org/eolang/printer/print/to-eo-tree.xsl")
            )
        )
    );

    /**
     * The default weights: an empty map, so every key uses its fallback.
     */
    private static final Map<PenaltyKey, Integer> DEFAULTS = Collections.emptyMap();

    /**
     * The XML.
     */
    private final XML xml;

    /**
     * The overridden penalty weights, by key.
     */
    private final Map<PenaltyKey, Integer> weights;

    /**
     * Ctor, using the default formatting weights.
     * @param src The source
     */
    public Xmir(final XML src) {
        this(src, Xmir.DEFAULTS);
    }

    /**
     * Ctor.
     * @param src The source
     * @param config The formatting weights; absent keys use their defaults
     */
    public Xmir(final XML src, final Map<PenaltyKey, Integer> config) {
        this.xml = src;
        this.weights = config;
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
     *
     * <p>The XMIR is first normalized by a train of XSL transformations
     * into an intermediate "line tree", which is then laid out as
     * pretty EO source by {@link Pretty}, using a penalty-based
     * algorithm to pick the most readable format for every object.</p>
     *
     * @return EO representation as {@link String}
     */
    public String toEO() {
        final XML xmir = Xmir.FOR_EO.pass(this.xml);
        Logger.debug(this, "XMIR after converting to EO tree:%n%s", xmir);
        return new Pretty(
            new Xnav(xmir.inner()).element("object").element("eo"),
            this.weights
        ).asString();
    }
}
