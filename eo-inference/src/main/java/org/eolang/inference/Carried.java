/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * What a void handed on to another void carries into it.
 *
 * <p>{@link Landed} gives no landing to a filling that runs into a void, and
 * {@link Fillings} therefore has a call site it cannot name a type for:
 * {@code inc y}, written inside a formation that declares {@code y}. What
 * arrives in {@code inc} is whatever arrives in {@code y}, one hop further
 * along, and the tables know that as soon as they know the one — so the hop is
 * walked here, and walked again, until no void learns anything new.</p>
 *
 * <p>A void whose source carries nothing carries nothing on, and that is not a
 * hole in the answer: an argument written in a formation nobody ever copies is
 * an argument nobody ever passes, and a program is looked at whole. So a void
 * filled with an {@code oak} once and handed on from a formation nobody calls
 * is filled with an {@code oak} and nothing else.</p>
 *
 * <p>Where the hop is all a void has, the far end of it is the answer, as
 * {@link Var}. That says less than a forma and more than silence, and it is
 * the truth about a pair of voids that are only ever filled together. It is
 * also the last resort, so where a forma does arrive it wins: {@code Φ.map}'s
 * inner {@code tup} used to be whatever {@code Φ.map.pairs} is and is now a
 * {@code Φ.tuple}.</p>
 *
 * @since 0.70.0
 */
final class Carried {

    /**
     * The types every void is filled with by a call site that names one.
     */
    private final Map<String, Map<String, Type>> placed;

    /**
     * The voids handed on to every void, as the variables they are, by the
     * locator of the void they were handed to.
     */
    private final Map<String, Map<String, Type>> handed;

    /**
     * Ctor.
     * @param named What every void is filled with where the filling has a type
     * @param hops The voids handed on to every void
     */
    Carried(
        final Map<String, Map<String, Type>> named,
        final Map<String, Map<String, Type>> hops
    ) {
        this.placed = named;
        this.handed = hops;
    }

    /**
     * What every void is filled with, the hops walked through.
     * @return The types, by name, by the locator of the void
     */
    Map<String, Map<String, Type>> all() {
        final Map<String, Map<String, Type>> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Map<String, Type>> hollow : this.placed.entrySet()) {
            found.put(hollow.getKey(), new LinkedHashMap<>(hollow.getValue()));
        }
        boolean grown = true;
        while (grown) {
            grown = false;
            for (final Map.Entry<String, Map<String, Type>> hop : this.handed.entrySet()) {
                if (this.arrived(hop.getKey(), found)) {
                    grown = true;
                }
            }
        }
        for (final Map.Entry<String, Map<String, Type>> hop : this.handed.entrySet()) {
            if (found.getOrDefault(hop.getKey(), Collections.emptyMap()).isEmpty()) {
                found.put(hop.getKey(), hop.getValue());
            }
        }
        return found;
    }

    private boolean arrived(final String hollow, final Map<String, Map<String, Type>> found) {
        final Map<String, Type> brought = new LinkedHashMap<>(0);
        for (final String source : this.handed.get(hollow).keySet()) {
            brought.putAll(found.getOrDefault(source, Collections.emptyMap()));
        }
        boolean grown = false;
        if (!brought.isEmpty()) {
            final Map<String, Type> into = found.computeIfAbsent(
                hollow, key -> new LinkedHashMap<>(0)
            );
            for (final Map.Entry<String, Type> type : brought.entrySet()) {
                if (into.putIfAbsent(type.getKey(), type.getValue()) == null) {
                    grown = true;
                }
            }
        }
        return grown;
    }
}
