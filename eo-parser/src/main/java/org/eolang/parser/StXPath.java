/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import java.util.List;
import java.util.function.Function;
import org.xembly.Directive;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Finds all XPath matches and replaces them
 * with what a function suggests.
 *
 * @since 0.29.0
 */
public final class StXPath implements Shift {
    /**
     * XPath to search for.
     */
    private final String xpath;

    /**
     * The mapping function.
     */
    private final Function<XML, Iterable<Directive>> fun;

    /**
     * Ctor.
     * @param path The XPath
     * @param func The function
     */
    public StXPath(final String path, final Function<XML, Iterable<Directive>> func) {
        this.xpath = path;
        this.fun = func;
    }

    @Override
    public String uid() {
        return this.getClass().getSimpleName();
    }

    @Override
    public XML apply(final int position, final XML xml) {
        final List<XML> nodes = xml.nodes(this.xpath);
        final XML doc;
        if (nodes.isEmpty()) {
            doc = xml;
        } else {
            final Directives dirs = new Directives();
            final String path = String.format("(%s)[1]", this.xpath);
            for (final XML node : nodes) {
                dirs.xpath(path).strict(1).append(this.fun.apply(node));
            }
            doc = new XMLDocument(
                new Xembler(dirs).applyQuietly(xml.inner())
            );
        }
        return doc;
    }
}
