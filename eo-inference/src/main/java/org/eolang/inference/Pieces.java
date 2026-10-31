/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * One line of source, cut into the pieces a page draws.
 *
 * <p>A line is text with objects written in it, and a page has to show both:
 * the text as the author typed it, and a mark on every stretch of it the table
 * has something to say about. Cutting the line up here rather than in the
 * stylesheet keeps the arithmetic of columns and widths in one place, where it
 * can be tested, and leaves the stylesheet nothing harder to do than putting a
 * class on a span.</p>
 *
 * <p>A chain of dispatches is the awkward case. {@code first.as-bytes.size}
 * is three objects and the XMIR gives all three the same column, the one where
 * the chain ends. So a group is walked leftward: the outermost keeps the
 * column it was given, and every object that is the receiver ({@code ρ}) of
 * the one before it takes the word before, back to the {@code ^} a chain is
 * often taken off. Anything else stays where the one before it was put and
 * shares its mark, which is right for a literal and the bytes it carries, and
 * for a step the source never wrote, such as the {@code dataized} a trailing
 * {@code !} slips into the middle of a chain. A formation is marked on the
 * name the arrow binds it to, since a bracket names nothing, and only a
 * formation bound to no name keeps its bracket marked.</p>
 *
 * <p>A mark over several objects tells only the one the source spells, as
 * {@link Written} chooses it, and takes its colour from that one alone.</p>
 *
 * @since 0.70.0
 */
final class Pieces {

    /**
     * The names that some letters on a page stand for.
     */
    private static final Map<String, String> SPELLED = Map.of(
        "T", "⊥", "^", "ρ", "Q", "Φ", "$", "ξ"
    );

    /**
     * The line, as the source wrote it.
     */
    private final String line;

    /**
     * The objects written on it.
     */
    private final Collection<Written> objects;

    /**
     * Ctor.
     *
     * @param text The line, as the source wrote it
     * @param written The objects written on it, in the order the XMIR has them
     */
    Pieces(final String text, final Collection<Written> written) {
        this.line = text;
        this.objects = written;
    }

    /**
     * Put the pieces of the line into a row of a page, in the order they are
     * read, a {@code bit} each.
     *
     * @param row The row, a {@code line} of a page
     * @param entries What a mark tells
     */
    void drawn(final Element row, final Entries entries) {
        final Document page = row.getOwnerDocument();
        final Map<Integer, List<Written>> laid = this.laid();
        final Reach reach = new Reach(this.line);
        int cursor = 0;
        for (final int column : new TreeSet<>(laid.keySet())) {
            final int width = reach.from(column);
            if (column >= cursor && width > 0) {
                if (column > cursor) {
                    row.appendChild(Pieces.plain(page, this.line.substring(cursor, column)));
                }
                final String text = this.line.substring(
                    column, Math.min(column + width, this.line.length())
                );
                final Element bit = page.createElement("bit");
                entries.told(bit, Pieces.spelled(laid.get(column), text).loc());
                final Element said = page.createElement("text");
                said.appendChild(page.createTextNode(text));
                bit.appendChild(said);
                row.appendChild(bit);
                cursor = column + width;
            }
        }
        if (cursor < this.line.length()) {
            row.appendChild(Pieces.plain(page, this.line.substring(cursor)));
        }
    }

    private static Element plain(final Document page, final String text) {
        final Element bit = page.createElement("bit");
        bit.appendChild(page.createTextNode(text));
        return bit;
    }

    private static Written spelled(final List<Written> said, final String text) {
        final String word = text.replaceFirst("^\\.+", "");
        final String spelt = Pieces.SPELLED.getOrDefault(word, word);
        return said.stream().min(
            Comparator.comparingInt((Written object) -> object.spelling(spelt))
                .thenComparingInt(Written::depth)
        ).orElseThrow();
    }

    private Map<Integer, List<Written>> laid() {
        final Map<Integer, List<Written>> grouped = new LinkedHashMap<>(0);
        for (final Written object : this.objects) {
            if (object.at() >= 0 && object.at() < this.line.length()) {
                grouped.computeIfAbsent(object.at(), key -> new ArrayList<>(1)).add(object);
            }
        }
        final Map<Integer, List<Written>> found = new LinkedHashMap<>(0);
        for (final Map.Entry<Integer, List<Written>> group : grouped.entrySet()) {
            final List<Written> chain = new ArrayList<>(group.getValue());
            chain.sort(Comparator.comparingInt(Written::hops));
            int place = this.bound(group.getKey());
            for (int step = 0; step < chain.size(); step = step + 1) {
                if (step > 0 && chain.get(step).receives(chain.get(step - 1))) {
                    place = this.leftward(place);
                }
                found.computeIfAbsent(place, key -> new ArrayList<>(1)).add(chain.get(step));
            }
        }
        return found;
    }

    private int bound(final int column) {
        int found = column;
        if (this.line.charAt(column) == '[') {
            int start = this.line.indexOf('>', column) + 1;
            while (start > 0 && start < this.line.length()
                && (this.line.charAt(start) == '>' || this.line.charAt(start) == ' ')) {
                start = start + 1;
            }
            if (start > 0 && start < this.line.length()) {
                found = start;
            }
        }
        return found;
    }

    private int leftward(final int column) {
        final int edge = Math.min(column, this.line.length());
        int start = edge;
        while (start > 0 && Pieces.wordy(this.line.charAt(start - 1))) {
            start = start - 1;
        }
        if (start == edge && start > 0 && this.line.charAt(start - 1) == '^') {
            start = start - 1;
        }
        final int found;
        if (start == edge) {
            found = column;
        } else if (start > 0 && this.line.charAt(start - 1) == '.') {
            found = start - 1;
        } else {
            found = start;
        }
        return found;
    }

    private static boolean wordy(final char glyph) {
        return Character.isLetterOrDigit(glyph) || glyph == '_' || glyph == '-';
    }
}
