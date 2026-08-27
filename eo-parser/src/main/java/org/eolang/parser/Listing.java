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
 * {@code <listing>…</listing>} element under {@code /object}.
 *
 * <p>The text is set as is, without any manual escaping: the XML writer
 * escapes it exactly once, so the text value of {@code /object/listing}
 * equals the source. A few characters are dropped first, for two
 * different reasons. The XML 1.1 restricted set — {@code 0x00-0x08},
 * {@code 0x0B-0x0C}, {@code 0x0E-0x1F}, {@code 0x7F-0x84} and
 * {@code 0x86-0x9F} — goes because {@link Directives#set(Object)}
 * refuses those and throws, whatever version the writer emits later.
 * {@code U+FFFE} and {@code U+FFFF} go because they are not XML
 * characters at all and would make the document not well-formed;
 * Xembly says nothing about them.</p>
 *
 * <p>The directives leave the cursor on {@code /object}, not inside the
 * {@code <listing>} they add, so that whatever the caller appends next
 * becomes a sibling of {@code <listing>} even without an absolute
 * {@code xpath()} reset of its own.</p>
 *
 * @since 0.1
 */
final class Listing implements Iterable<Directive> {

    /**
     * Characters that must not reach an XML text node: the XML 1.1
     * restricted set, which Xembly refuses, plus {@code U+FFFE} and
     * {@code U+FFFF}, which are not XML characters.
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
            .up()
            .iterator();
    }
}
