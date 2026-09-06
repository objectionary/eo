/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * The names every filling of a void answers the same way, where the program
 * fills it several ways.
 *
 * <p>{@link Sole} and {@link Shared} name a void as a whole, and where the
 * fillings share no ancestor there is no such name: a void holding a
 * {@code Φ.file} and a {@code Φ.directory.tmpfile} is neither of them and is
 * not a third thing either, so a dispatch rooted at it stops there and
 * {@link Dispatched} invents a name nothing has a row for.</p>
 *
 * <p>Sharing no ancestor is not the same as having nothing in common. Both of
 * those fillings answer {@code size} with the same {@code Φ.file.size}, one of
 * them because it says so and the other because it cannot answer at all, and a
 * name they agree on is not picking a favourite among facts. It is asked of
 * every filling that has a row, and taken when one answer comes back — a
 * filling that says nothing about the name says nothing against it, since most
 * rows are incomplete and silence is not denial.</p>
 *
 * <p>An answer with no row of its own is silence as well. {@link Provided}
 * invents a member for a void it walks through, the way it names the
 * {@code size} of a {@code Φ.directory.tmpfile} after the void that object
 * delegates into, and a made-up name disagreeing with a real one would sink
 * every question the two fillings do agree on. It is the same demand
 * {@link Sole} and {@link Shared} make of what they name: a locator a reader
 * can go and look at, since whoever reads on needs a row to read the next
 * answer off.</p>
 *
 * <p>What comes back is a fact about the dispatch and not about the void,
 * which keeps the standing {@link Shared} already has: the void is still
 * whatever the next caller puts there, and the answer is a waypoint the chain
 * carries on from rather than a name for the hole.</p>
 *
 * @since 0.72.0
 */
final class Agreed {

    /**
     * What the program was seen putting into the void, from {@link Fillings}.
     */
    private final Collection<Type> told;

    /**
     * The objects the provides table has a row for.
     */
    private final Collection<String> known;

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * Ctor.
     * @param witnesses What the program was seen putting into the void, from
     *  {@link Fillings}
     * @param objects The objects the provides table has a row for
     * @param provided What the types certainly have
     */
    Agreed(
        final Collection<Type> witnesses,
        final Collection<String> objects,
        final Provided provided
    ) {
        this.told = witnesses;
        this.known = objects;
        this.owned = provided;
    }

    /**
     * The names taken off this void that every filling answers the same way.
     * @param hollow The locator of the void
     * @param asked The names the program takes off it, without the void in
     *  front of them
     * @return The object each name is, by the locator the name goes by, empty
     *  where the fillings answer none of them with one voice
     */
    Map<String, String> members(final String hollow, final Collection<String> asked) {
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final String name : asked) {
            final Collection<String> replies = this.replies(name);
            final String member = String.join(".", hollow, name);
            if (replies.size() == 1 && !replies.contains(member)) {
                found.put(member, replies.iterator().next());
            }
        }
        return found;
    }

    private Collection<String> replies(final String name) {
        final Collection<String> found = new LinkedHashSet<>(0);
        for (final Type one : this.told) {
            if (this.known.contains(one.names())) {
                found.add(this.owned.attribute(one.names(), name));
            }
        }
        found.retainAll(this.known);
        return found;
    }
}
