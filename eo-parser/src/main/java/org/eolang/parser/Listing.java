/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Iterator;
import java.util.regex.Pattern;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Source-text {@code <listing>} directive — wraps the original EO
 * source as an {@code Iterable<Directive>} that appends a
 * {@code <listing>…&lt;/listing>} element under {@code /object}.
 *
 * <p>The text is set as is, without any manual escaping: the XML writer
 * escapes it exactly once, so the text value of {@code /object/listing}
 * equals the source. Only the characters that XML forbids in text nodes
 * are dropped, since they can't be represented there at all. See this
 * <a href="https://github.com/objectionary/eo/issues/6228">issue</a> for
 * more details.</p>
 *
 * @since 0.1
 */
final class Listing implements Iterable<Directive> {

    /**
     * Characters that are not allowed inside an XML text node.
     */
    private static final Pattern FORBIDDEN = Pattern.compile(
        "[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F-\\x84\\x86-\\x9F\\uFFFE\\uFFFF]"
    );

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
            .set(Listing.FORBIDDEN.matcher(this.source).replaceAll(""))
            .iterator();
    }
}
