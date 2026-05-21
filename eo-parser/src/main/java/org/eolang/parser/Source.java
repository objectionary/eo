/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * EO source text as an iterable of {@link Span}.
 *
 * <p>The source is UTF-8 by contract (R-2.1.1) — caller-supplied as a
 * decoded {@link String}, so encoding handling lives in the layer above.
 * Line endings {@code \n} and {@code \r\n} are normalised to {@code \n}
 * (R-2.1.2); a bare {@code \r} also terminates a line. The final line need
 * not carry a terminator.</p>
 *
 * <p>Spans are produced in source order, numbered from 1. An empty input
 * yields no spans. An input that is a single empty line yields one blank
 * {@link Span}. *
 *
 * @since 0.1
 */
final class Source implements Iterable<Span> {

    /**
     * Raw source text (already decoded; UTF-8 is the caller's contract).
     */
    private final String text;

    /**
     * Ctor.
     * @param raw The full source text
     */
    Source(final String raw) {
        this.text = raw;
    }

    @Override
    public Iterator<Span> iterator() {
        return this.spans().iterator();
    }

    /**
     * Materialise all spans.
     *
     * <p>The walk is linear in input length. Splitting eagerly costs O(N)
     * once, vs. O(N) over the consumer's loop; the simpler shape is worth
     * the small allocation.</p>
     *
     * @return All source lines as spans in source order
     */
    private List<Span> spans() {
        final List<Span> out = new ArrayList<>(this.text.length() / 32 + 1);
        final int len = this.text.length();
        int start = 0;
        int number = 1;
        int pos = 0;
        while (pos < len) {
            final char glyph = this.text.charAt(pos);
            if (glyph == '\n') {
                out.add(new Span(this.text.substring(start, pos), number));
                number = number + 1;
                pos = pos + 1;
                start = pos;
            } else if (glyph == '\r') {
                out.add(new Span(this.text.substring(start, pos), number));
                number = number + 1;
                pos = pos + 1;
                if (pos < len && this.text.charAt(pos) == '\n') {
                    pos = pos + 1;
                }
                start = pos;
            } else {
                pos = pos + 1;
            }
        }
        if (start < len) {
            out.add(new Span(this.text.substring(start, len), number));
        }
        return out;
    }
}
