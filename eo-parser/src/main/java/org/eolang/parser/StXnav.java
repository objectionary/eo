/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import java.util.function.Consumer;
import org.w3c.dom.Node;

/**
 * This {@link Shift} finds all XPath matches and replaces them
 * with what a function suggests using {@link com.github.lombrozo.xnav.Xnav}.
 *
 * @since 0.53.0
 */
final class StXnav implements Shift {
    /**
     * XPath to search for.
     */
    private final String xpath;

    /**
     * The mapping function.
     */
    private final Consumer<Xnav> fun;

    /**
     * UID.
     */
    private final String identifier;

    /**
     * Ctor.
     * @param path The XPath
     * @param func The function
     */
    StXnav(final String path, final Consumer<Xnav> func) {
        this("st-xnav", path, func);
    }

    /**
     * Ctor.
     * @param identifier UID
     * @param path The XPath
     * @param func The function
     */
    StXnav(final String identifier, final String path, final Consumer<Xnav> func) {
        this.identifier = identifier;
        this.xpath = path;
        this.fun = func;
    }

    @Override
    public String uid() {
        return this.identifier;
    }

    @Override
    public XML apply(final int position, final XML xml) {
        final Node dom = xml.inner();
        new Xnav(dom).path(this.xpath).forEach(this.fun);
        return new XMLDocument(dom);
    }
}
