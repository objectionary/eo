/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.function.Supplier;

/**
 * One glyph spelled the way an EO string literal carries it.
 *
 * <p>A quote, a backslash and a control character cannot stand inside a
 * literal as they are: the quote closes it, the backslash swallows its
 * neighbour, and a line feed breaks the literal in two. Each of them turns
 * into the escape sequence the parser decodes back into the very same
 * glyph (R-9.7), while everything else stays verbatim.</p>
 *
 * <p>The glyphs from {@code 0x08} to {@code 0x0D} carry the one-letter
 * spellings of {@code "btnvfr"}, except the vertical tab {@code 0x0B},
 * which EO has no letter for and which goes out as a unicode escape.</p>
 *
 * @since 0.73.3
 */
final class Escaped implements Supplier<String> {

    /**
     * The glyph.
     */
    private final char glyph;

    /**
     * Ctor.
     *
     * @param glyph The glyph
     */
    Escaped(final char glyph) {
        this.glyph = glyph;
    }

    @Override
    public String get() {
        final int code = this.glyph;
        final String text;
        if (code == '"' || code == '\\') {
            text = String.format("\\%c", this.glyph);
        } else if (code >= 0x08 && code <= 0x0D && code != 0x0B) {
            text = String.format("\\%c", "btnvfr".charAt(code - 0x08));
        } else if (code < 0x20 || code == 0x7F) {
            text = String.format("\\u%04x", code);
        } else {
            text = String.valueOf(this.glyph);
        }
        return text;
    }
}
