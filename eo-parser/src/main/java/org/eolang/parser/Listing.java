/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Iterator;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Source-text {@code <listing>} directive — wraps the original EO
 * source as an {@code Iterable<Directive>} that appends a
 * {@code <listing>…</listing>} element under {@code /object}.
 *
 * <p>The text is set as is, without any manual escaping: the XML writer
 * escapes it exactly once, so the text value of {@code /object/listing}
 * equals the source. A few characters are dropped first, by
 * {@link Scrubbed}, because an XML text node cannot hold them.</p>
 *
 * <p>The directives leave the cursor on {@code /object}, not inside the
 * {@code <listing>} they add, so that whatever the caller appends next
 * becomes a sibling of {@code <listing>} even without an absolute
 * {@code xpath()} reset of its own.</p>
 *
 * <p>When nothing survives the scrub — the source is empty or made
 * entirely of forbidden characters — no element is appended at all.
 * {@code XMIR.xsd} types {@code listing} as {@code non-empty}, so an
 * empty {@code <listing/>} is not a valid instance of it; the schema
 * allows the element to be omitted instead
 * ({@code minOccurs="0"}).</p>
 *
 * @since 0.1
 */
final class Listing implements Iterable<Directive> {

    /**
     * Raw EO source text to embed verbatim under {@code <listing>}.
     */
    private final String source;

    /**
     * Ctor.
     *
     * @param text The source text
     */
    Listing(final String text) {
        this.source = text;
    }

    @Override
    public Iterator<Directive> iterator() {
        final String text = new Scrubbed(this.source).toString();
        final Directives dirs = new Directives();
        if (!text.isEmpty()) {
            dirs.xpath("/object").strict(1).add("listing").set(text).up();
        }
        return dirs.iterator();
    }
}
