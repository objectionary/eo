/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.xembly.Directives;

/**
 * One line of source, cut into the pieces a page draws.
 *
 * <p>A line is text with objects written in it, and a page has to show both:
 * the text as the author typed it, and a mark on every stretch of it that the
 * tables have something to say about. Cutting the line up here rather than in
 * the stylesheet keeps the arithmetic of columns and widths in one place,
 * where it can be tested, and leaves the page nothing harder to do than
 * putting a class on a span.</p>
 *
 * <p>A mark carries what the tables said about every object under it, so that
 * a reader hovering over a word is told the same thing the tables hold: the
 * name it goes by, what it settled on, and, where it settled on nothing better
 * than somebody else's void, what the program was seen putting into that
 * void. An object whose answer is its own locator is a void itself and is
 * marked as one, since telling a reader that {@code args} is whatever
 * {@code args} turns out to be is telling them nothing.</p>
 *
 * <p>A chain of dispatches is the awkward case. {@code first.as-bytes.size}
 * is three objects and the XMIR gives all three the same column, because each
 * one is written where the chain ends rather than where it begins. So a group
 * is walked leftward instead: the outermost keeps the column it was given, and
 * every object that is the receiver ({@code ρ}) of the one before it takes the
 * word before, back to the {@code ^} a chain is often taken off. Anything else
 * stays where the one before it was put and shares its mark, which is right
 * for a literal and the bytes it carries — to a reader those are one thing
 * written once — and right for a step whose own receiver the source never
 * wrote, such as the {@code dataized} that a trailing {@code !} slips into the
 * middle of a chain, since a reader has no word to hang it on.</p>
 *
 * <p>A mark shared that way is coloured by the worst object under it. A
 * stretch of text with something unknown beneath it has to say so: colouring
 * it by whichever object came first drew green what the tally counts as
 * unknown, and the tally is counted from these same answers for exactly that
 * reason. The body of an atom sits under the bracket that opens it, so an
 * atom nobody can answer for used to be invisible on the page while being
 * counted at the top of it (#8318). Which object the warning is about the
 * reader finds in the popup, where all of them are listed.</p>
 *
 * @since 0.70.0
 */
final class Pieces {

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
     * @param text The line, as the source wrote it
     * @param written The objects written on it, in the order the XMIR has them
     */
    Pieces(final String text, final Collection<Written> written) {
        this.line = text;
        this.objects = written;
    }

    /**
     * The pieces of the line, in the order they are read.
     * @return The directives, one {@code bit} per piece
     */
    Directives directives() {
        final Map<Integer, Collection<Written>> laid = this.laid();
        final List<Integer> columns = new ArrayList<>(laid.keySet());
        Collections.sort(columns);
        final Reach reach = new Reach(this.line);
        final Directives dirs = new Directives();
        int cursor = 0;
        for (final int column : columns) {
            final int width = reach.from(column);
            if (column >= cursor && width > 0) {
                if (column > cursor) {
                    dirs.add("bit").set(this.line.substring(cursor, column)).up();
                }
                dirs.append(
                    Pieces.marked(
                        this.line.substring(column, Math.min(column + width, this.line.length())),
                        laid.get(column)
                    )
                );
                cursor = column + width;
            }
        }
        if (cursor < this.line.length()) {
            dirs.add("bit").set(this.line.substring(cursor)).up();
        }
        return dirs;
    }

    private Map<Integer, Collection<Written>> laid() {
        final Map<Integer, Collection<Written>> found = new LinkedHashMap<>(0);
        for (final Map.Entry<Integer, Collection<Written>> group : this.grouped().entrySet()) {
            this.walked(found, group.getKey(), Pieces.sorted(group.getValue()));
        }
        return found;
    }

    private Map<Integer, Collection<Written>> grouped() {
        final Map<Integer, Collection<Written>> found = new LinkedHashMap<>(0);
        for (final Written object : this.objects) {
            if (object.at() >= 0 && object.at() < this.line.length()) {
                found.computeIfAbsent(object.at(), key -> new ArrayList<>(1)).add(object);
            }
        }
        return found;
    }

    private void walked(
        final Map<Integer, Collection<Written>> found,
        final int column,
        final List<Written> chain
    ) {
        int place = column;
        for (int step = 0; step < chain.size(); step = step + 1) {
            final Written link = chain.get(step);
            if (step > 0 && link.loc().equals(chain.get(step - 1).loc().concat(".ρ"))) {
                place = this.leftward(place);
            }
            found.computeIfAbsent(place, key -> new ArrayList<>(1)).add(link.moved(place));
        }
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
        } else {
            if (start > 0 && this.line.charAt(start - 1) == '.') {
                start = start - 1;
            }
            found = start;
        }
        return found;
    }

    private static List<Written> sorted(final Collection<Written> group) {
        final List<Written> found = new ArrayList<>(group);
        found.sort(
            (first, second) -> Integer.compare(
                Pieces.hops(first.loc()), Pieces.hops(second.loc())
            )
        );
        return found;
    }

    private static int hops(final String locator) {
        int found = 0;
        int spot = locator.indexOf(".ρ");
        while (spot >= 0) {
            found = found + 1;
            spot = locator.indexOf(".ρ", spot + 1);
        }
        return found;
    }

    private static Directives marked(final String text, final Collection<Written> said) {
        final Directives dirs = new Directives()
            .add("bit")
            .attr("band", Pieces.band(Pieces.worst(said)));
        for (final Written object : said) {
            dirs.add("told")
                .attr("label", object.label())
                .attr("band", Pieces.band(object.answer().rung()))
                .attr("where", object.answer().where())
                .attr("loc", object.loc());
            if (object.loc().equals(object.answer().where())
                && "rooted".equals(Pieces.band(object.answer().rung()))) {
                dirs.attr("void", "true");
            }
            Pieces.witnessed(dirs, object.answer().seen());
            dirs.up();
        }
        return dirs.add("text").set(text).up().up();
    }

    private static void witnessed(final Directives dirs, final Collection<Type> seen) {
        if (!seen.isEmpty()) {
            dirs.add("seen");
            for (final Type witness : seen) {
                dirs.append(witness.directives());
            }
            dirs.up();
        }
    }

    private static int worst(final Collection<Written> said) {
        int found = Integer.MAX_VALUE;
        for (final Written object : said) {
            found = Math.min(found, object.answer().rung());
        }
        return found;
    }

    private static String band(final int rung) {
        final String found;
        if (rung == 0) {
            found = "blank";
        } else if (rung == 1) {
            found = "rooted";
        } else {
            found = "named";
        }
        return found;
    }

    private static boolean wordy(final char glyph) {
        return Character.isLetterOrDigit(glyph) || glyph == '_' || glyph == '-';
    }
}
