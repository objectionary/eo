/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * What a void the program fills one way is worth to the dispatches rooted at
 * it.
 *
 * <p>A dispatch on a void is answered by the void's own name and no further:
 * {@code x.leaf} inside a formation that takes {@code x} is a different object
 * for every caller, so {@link Dispatched} calls it the {@code leaf} of
 * {@code Φ.inc.x} and stops, having nowhere to look up what a {@code leaf} is.
 * Where every caller in the program puts an {@code oak} into that void there is
 * somewhere to look, and the next dispatch along the chain is answered too — the
 * point of the whole exercise, since a chain that stops at the first step
 * carries no further facts.</p>
 *
 * <p>{@link Sole} decides what a void is worth and this asks it, of every void
 * at once, off the table {@link Woven} builds from the pairs settled so far.
 * Being asked off a table is what makes the answer improve as the passes go on:
 * an argument whose own type was worked out this pass is a filling with a type
 * next pass, and a void nothing could be said about becomes a void filled one
 * way.</p>
 *
 * <p>A void keeps itself all the same, and none of this is ever written down as
 * a row. What goes into a void is gathered from its callers and is a fact about
 * them; the void is still whatever the next caller puts there, and a row saying
 * it is a copy of an {@code oak} would tell a reader of the tables something the
 * program does not say. What is written down is the dispatches this answers,
 * which are facts about the objects they are dispatches of.</p>
 *
 * <p>Only what the tables have seen is counted, and they have not seen
 * everything: a caller that passes on a void of its own is a caller nobody has
 * looked into, and {@link Fillings} leaves it out of the choice without saying
 * that it did (#8226). So a void with one witness and such a caller is promoted
 * here as though the one witness were all there is.</p>
 *
 * @since 0.71.0
 */
final class Promoted {

    /**
     * The rows that follow from a set of pairs.
     */
    private final Woven table;

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * The rows of the links table that are not pairs, from {@link Pairs}.
     */
    private final Map<String, Type> others;

    /**
     * Ctor.
     * @param woven The rows that follow from a set of pairs
     * @param provides The provides table, which says where a filling can land
     * @param kept The rows of the links table that are not pairs, without which
     *  a filling that arrives at a literal is not seen to be one
     */
    Promoted(final Woven woven, final XML provides, final Map<String, Type> kept) {
        this.table = woven;
        this.given = provides;
        this.others = kept;
    }

    /**
     * The voids these pairs turn out to have named, beyond the ones named
     * already.
     * @param pairs The pairs, each object against the one it is a copy of
     * @return The voids answered this time, each against the one object the
     *  program puts into it, empty when no void is worth anything further
     */
    Map<String, String> from(final Map<String, String> pairs) {
        final Collection<String> known = this.known();
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Collection<Type>> hollow
            : new Fillings(this.links(pairs), this.given).all().entrySet()) {
            final String sole = new Sole(hollow.getValue(), known).names();
            if (!sole.isEmpty() && !pairs.containsKey(hollow.getKey())) {
                found.put(hollow.getKey(), sole);
            }
        }
        return found;
    }

    private XML links(final Map<String, String> pairs) {
        final Map<String, Type> rows = this.table.rows(pairs);
        rows.putAll(this.others);
        return new Types(rows).asXml();
    }

    private Collection<String> known() {
        final Collection<String> found = new HashSet<>(0);
        for (final Xnav type : new Rows(this.given).all()) {
            found.add(new Noted(type).says("id"));
        }
        return found;
    }
}
