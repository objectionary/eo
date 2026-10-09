/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.Sources;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamSource;

/**
 * The files the stylesheets of this module read, found by the links to them.
 *
 * <p>A stylesheet of this module reads two kinds of files. Another
 * stylesheet, which it includes to share a function, is named by its path
 * inside this module, such as {@code /org/eolang/lowering/_returns.xsl},
 * and is read from the classpath, since it is packed into the jar of this
 * module. A table, which it opens while it runs, such as a table of
 * {@code eo:inference}, is named by its whole URI, and is read from the
 * disk.</p>
 *
 * @since 0.64.0
 */
final class Hrefs implements Sources {

    /**
     * Where the stylesheets of this module are read from.
     */
    private final Sources classpath;

    /**
     * Ctor.
     */
    Hrefs() {
        this(new ClasspathSources());
    }

    /**
     * Ctor.
     *
     * @param sheets Where the stylesheets of this module are read from
     */
    Hrefs(final Sources sheets) {
        this.classpath = sheets;
    }

    @Override
    public Source resolve(final String href, final String base) throws TransformerException {
        final Source found;
        if (href.startsWith("/")) {
            found = this.classpath.resolve(href, base);
        } else {
            found = new StreamSource(href);
        }
        return found;
    }
}
