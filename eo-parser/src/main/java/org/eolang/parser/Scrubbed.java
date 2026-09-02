/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Source text an XML text node can hold — the same text without the
 * characters such a node must not carry.
 *
 * <p>The XML 1.1 restricted set goes because
 * {@link org.xembly.Directives#set(Object)} refuses it and throws,
 * whatever version the writer emits later; {@code U+FFFE} and
 * {@code U+FFFF} go because they are not XML characters at all. Every
 * place that hands raw source text to {@code set()} must wrap it in
 * this, or a control character in the source breaks the parse instead
 * of being reported by it (#7825).</p>
 *
 * @since 0.62.2
 */
final class Scrubbed {

    /**
     * Characters that must not reach an XML text node.
     */
    private static final Pattern FORBIDDEN = Pattern.compile(
        "[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F-\\x84\\x86-\\x9F\\uFFFE\\uFFFF]"
    );

    /**
     * The text as it came from the source.
     */
    private final String origin;

    /**
     * Ctor.
     * @param text The text as it came from the source
     */
    Scrubbed(final String text) {
        this.origin = text;
    }

    @Override
    public String toString() {
        return Scrubbed.FORBIDDEN.matcher(this.origin).replaceAll("");
    }

    /**
     * Where the first character an XML node cannot hold sits.
     *
     * <p>Text is scrubbed of them, but a name is not: it becomes an
     * attribute of a generated object, and a name with a hole in it is
     * not the name the source wrote. A reader of an identifier asks this
     * instead, and reports the character at the column it sits in
     * (#7927).</p>
     *
     * @return Index of the first such character, or -1 when there is none
     */
    int found() {
        final Matcher matcher = Scrubbed.FORBIDDEN.matcher(this.origin);
        final int index;
        if (matcher.find()) {
            index = matcher.start();
        } else {
            index = -1;
        }
        return index;
    }
}
