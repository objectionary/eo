/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

/**
 * EO source text as an iterable of {@link Span}.
 *
 * <p>The source is UTF-8 by contract (R-2.1.1) — caller-supplied as a
 * decoded {@link String}, so encoding handling lives in the layer above.
 * Line endings {@code \n} and {@code \r\n} are normalised to {@code \n}
 * (R-2.1.2), and those two are the only ones: a {@code \r} that no
 * {@code \n} follows stays inside the line, where {@link Eo} rejects it.
 * The final line need not carry a terminator.</p>
 *
 * <p>Spans are produced in source order, numbered from 1. An empty input
 * yields no spans. An input that is a single empty line yields one blank
 * {@link Span}.</p>
 *
 * @since 0.1
 */
final class Source implements Iterable<Span> {

    /**
     * All spans of the source, in source order: split once, on the first
     * iteration, and reused by every iteration after it.
     */
    private final Unchecked<List<Span>> lines;

    /**
     * Ctor.
     * @param raw The full source text
     */
    Source(final String raw) {
        this.lines = new Unchecked<>(new Sticky<>(() -> Source.spans(raw)));
    }

    @Override
    public Iterator<Span> iterator() {
        return this.lines.value().iterator();
    }

    private static List<Span> spans(final String text) {
        final List<Span> out = new ArrayList<>(text.length() / 32 + 1);
        final int len = text.length();
        int start = 0;
        int number = 1;
        int pos = 0;
        while (pos < len) {
            final char glyph = text.charAt(pos);
            if (glyph == '\n') {
                out.add(new Span(text.substring(start, pos), number));
                number = number + 1;
                pos = pos + 1;
                start = pos;
            } else if (glyph == '\r' && pos + 1 < len && text.charAt(pos + 1) == '\n') {
                out.add(new Span(text.substring(start, pos), number));
                number = number + 1;
                pos = pos + 2;
                start = pos;
            } else {
                pos = pos + 1;
            }
        }
        if (start < len) {
            out.add(new Span(text.substring(start, len), number));
        }
        return Collections.unmodifiableList(out);
    }
}
