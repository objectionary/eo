/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Which answers came back with a choice between several objects.
 *
 * <p>A call on a void that holds a picker hands back one of the arguments it
 * was given, and where the arms agree on nothing the row names all of them
 * rather than none (#8744). The walk that puts an object on a rung never sees
 * that: it ends at the void either way, so an object told it is either a
 * {@code Φ.dial} or a {@code Φ.clock} is counted beside an object told
 * nothing, and a branch that moves hundreds of rows from the second to the
 * first prints numbers identical to the ones before it (#8854).</p>
 *
 * <p>So the arms are stamped on here, once the walk is over, the way
 * {@link Forged} stamps the voids only an atom fills. They are not something
 * the walk found out — {@link Dispatched} worked them out passes earlier and
 * the table has held them ever since — and the walk has no business carrying a
 * fact it never uses.</p>
 *
 * @since 0.71.0
 */
final class Chosen {

    /**
     * The arms of every row that came back with several objects.
     */
    private final Map<String, Collection<Type>> picked;

    /**
     * Ctor.
     *
     * @param arms The arms of every row that came back with several objects,
     *  from {@link Pairs#arms()}
     */
    Chosen(final Map<String, Collection<Type>> arms) {
        this.picked = arms;
    }

    /**
     * Stamp the answers that came back with a choice.
     *
     * @param told The answers, by the locator of the object
     * @return The same answers, with the ones naming several objects carrying
     *  them
     */
    Map<String, Answer> marked(final Map<String, Answer> told) {
        final Map<String, Answer> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Answer> object : told.entrySet()) {
            final Collection<Type> arms =
                this.picked.getOrDefault(object.getKey(), Collections.emptyList());
            final Answer answer = object.getValue();
            if (arms.isEmpty()) {
                found.put(object.getKey(), answer);
            } else {
                found.put(
                    object.getKey(),
                    new Answer(
                        answer.where(), answer.rung(), answer.seen(), answer.forged(), arms
                    )
                );
            }
        }
        return found;
    }
}
