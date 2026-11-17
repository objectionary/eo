/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * One segment of an object name, as a Java identifier.
 *
 * <p>The segment is escaped the way {@code eo:clean} escapes it in
 * {@code _java-names.xsl}, which the transpiler uses to name the class it
 * writes, so that a name the transpiler accepted is found at runtime. The
 * dollar is escaped ahead of everything else, so that an escape is never
 * read as a dollar the name itself carried. The dash then becomes an
 * underscore and the underscore an escape of its own, which no dash can
 * produce, so that "a-_b" and "a_-b" stay two names. Every other character
 * Java forbids in an identifier becomes an escape of six digits, not four,
 * since four drop everything above the sixteenth bit.</p>
 *
 * @since 0.64.0
 */
final class Cleaned {

    /**
     * The segment, as the object name spells it.
     */
    private final String segment;

    /**
     * Ctor.
     *
     * @param seg The segment of the object name
     */
    Cleaned(final String seg) {
        this.segment = seg;
    }

    @Override
    public String toString() {
        final String dollared = this.segment.replace("$", "$EO");
        final StringBuilder out = new StringBuilder(dollared.length());
        int idx = 0;
        while (idx < dollared.length()) {
            final int code = dollared.codePointAt(idx);
            if (code == '-' || code == 'α') {
                out.append('_');
            } else if (code == '@') {
                out.append('φ');
            } else if (Character.isLetterOrDigit(code) || code == '$') {
                out.appendCodePoint(code);
            } else {
                out.append(String.format("$u%06X", code));
            }
            idx += Character.charCount(code);
        }
        return out.toString();
    }
}
