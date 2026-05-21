/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Iterator;
import org.apache.commons.text.StringEscapeUtils;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Source-text {@code <listing>} directive — wraps the original EO
 * source as an {@code Iterable<Directive>} that appends a
 * {@code <listing>…&lt;/listing>} element under {@code /object}.
 * @since 0.1
 */
final class Listing implements Iterable<Directive> {

    /**
     * Raw EO source text to embed verbatim under {@code <listing>}.
     */
    private final String source;

    /**
     * Ctor.
     * @param text The source text
     */
    Listing(final String text) {
        this.source = text;
    }

    @Override
    public Iterator<Directive> iterator() {
        return new Directives()
            .xpath("/object")
            .strict(1)
            .add("listing")
            .set(StringEscapeUtils.escapeXml11(this.source))
            .iterator();
    }
}
