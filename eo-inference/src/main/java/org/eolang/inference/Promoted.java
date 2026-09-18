/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
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
 * at once, off what {@link Woven} works out the pairs settled so far to have
 * put where. That is the table the build goes on to publish, said as
 * {@link Said} rather than written as a document, since a pass that rendered
 * it only to read it back would spend most of itself doing so.
 * {@link Shared} is asked where the fillings share an ancestor and {@link
 * Agreed} where they share nothing but their answers, so a void nothing can be
 * called as a whole still carries the dispatches its fillings agree on. Which
 * names those are is read back off the answers themselves, since a dispatch a
 * void leaves unanswered is answered with a name rooted at the void. Only the
 * first hop is read: what a further hop asks is asked of an answer nobody has
 * settled yet, and once this settles the first the next pass reaches the
 * second.
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
 * <p>A caller that passes on a void of its own names no type, and that leaves
 * no gap in the choice: {@link Carried} walks the hop until no void learns
 * anything new, and where a forma arrives as well it beats the hop, so a void
 * filled with an {@code oak} once and handed on from a formation nobody copies
 * is filled with an {@code oak} and nothing else (#8229). Where the hop is all
 * a void has, what comes back is a {@link Var}, which names nothing a reader
 * could go and look at, and {@link Sole} refuses it.</p>
 *
 * <p>A filling that waits on the very void it fills is settled first, by
 * {@link Freed}, so that the two are not left waiting on each other for as
 * long as the passes run.</p>
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
     * What the links table says, as the rules left it.
     */
    private final Said written;

    /**
     * The locator of every void.
     */
    private final Collection<String> hollows;

    /**
     * The arguments of every application, from {@link Given}.
     */
    private final Map<String, List<String>> arms;

    /**
     * Ctor.
     *
     * @param woven The rows that follow from a set of pairs
     * @param provides The provides table, which says where a filling can land
     * @param said What the links table says, as the rules left it, without
     *  which a filling that arrives at a literal is not seen to be one
     * @param voids The locator of every void, from {@link Hollows}
     * @param arguments The arguments of every application, without which a
     *  filling that waits on the void it fills is not seen to be a choice
     */
    Promoted(
        final Woven woven,
        final XML provides,
        final Said said,
        final Collection<String> voids,
        final Map<String, List<String>> arguments
    ) {
        this.table = woven;
        this.given = provides;
        this.written = said;
        this.hollows = voids;
        this.arms = arguments;
    }

    /**
     * The voids these pairs turn out to have named, beyond the ones named
     * already.
     *
     * @param pairs The pairs, each object against the one it is a copy of
     * @return The voids answered this time, each against the one object the
     *  program puts into it, empty when no void is worth anything further
     */
    Map<String, String> from(final Map<String, String> pairs) {
        final Collection<String> known = this.known();
        final Provided owned = new Provided(
            this.given, new Ends(pairs).names(), this.hollows
        );
        final Map<String, Collection<String>> asked = this.asked(pairs);
        final Said said = this.written.with(pairs, this.table.binds(pairs));
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Collection<Type>> hollow
            : new Freed(
                new Fillings(said, this.given, this.hollows).all(), said, owned, this.arms
            ).all().entrySet()) {
            String sole = new Sole(hollow.getValue(), known).names();
            if (sole.isEmpty()) {
                sole = new Shared(hollow.getValue(), known, owned).names();
            }
            if (sole.isEmpty()) {
                found.putAll(
                    new Agreed(hollow.getValue(), known, owned).members(
                        hollow.getKey(),
                        asked.getOrDefault(hollow.getKey(), Collections.emptyList())
                    )
                );
            } else {
                found.put(hollow.getKey(), sole);
            }
        }
        found.keySet().removeAll(pairs.keySet());
        return found;
    }

    private Map<String, Collection<String>> asked(final Map<String, String> pairs) {
        final Rooted rooted = new Rooted(this.hollows);
        final Map<String, Collection<String>> found = new LinkedHashMap<>(0);
        for (final String answer : pairs.values()) {
            final String hollow = rooted.names(answer);
            if (!hollow.isEmpty() && answer.length() > hollow.length()) {
                final String name = answer.substring(hollow.length() + 1);
                if (!name.contains(".")) {
                    found.computeIfAbsent(hollow, key -> new HashSet<>(0)).add(name);
                }
            }
        }
        return found;
    }

    private Collection<String> known() {
        final Collection<String> found = new HashSet<>(0);
        for (final Xnav type : new Rows(this.given).all()) {
            found.add(new Noted(type).says("id"));
        }
        return found;
    }
}
