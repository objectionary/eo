/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * What was put into one void, each thing counted once for the type it is.
 *
 * <p>{@link Fillings} keeps a filling by the name the walk gave it, which is
 * the locator of an object, and two objects can be one type: a
 * {@code Φ.bytes.as-bytes} hands back the object it was dispatched on and is a
 * {@code Φ.bytes}, so a void filled with one of those here and with a plain
 * {@code Φ.bytes} there is filled one way twice. Counted apart they leave the
 * void holding a choice between two names for one thing, which nobody can
 * answer and which {@link Sole} rightly refuses. {@link Behaved} works out
 * that name and {@link Reduced} writes it on the row, so the fact is in the
 * table before anybody counts.</p>
 *
 * <p>The counting waits until the walk is over. What is folded together here
 * is a name, and the objects behind it stay apart for as long as anybody has
 * business with them: {@link Handed} gives a chunk to the first void of every
 * formation an atom is handed, and two scopes that behave alike are still two
 * scopes, each with a void of its own to fill. Fold them early and eleven of
 * them are left holding nothing.</p>
 *
 * @since 0.73.0
 */
final class Counted {

    /**
     * What was put in, by the name the walk gave it, from {@link Fillings}.
     */
    private final Map<String, Type> told;

    /**
     * The name every type behaves as, from {@link Behaviours}.
     */
    private final Map<String, String> behaves;

    /**
     * Ctor.
     *
     * @param members What was put in, by the name the walk gave it
     * @param reduced The name every type behaves as, empty where the table has
     *  not been asked the question yet
     */
    Counted(final Map<String, Type> members, final Map<String, String> reduced) {
        this.told = members;
        this.behaves = reduced;
    }

    /**
     * What was put in, one member to a type.
     *
     * @return The types, in the order they were first seen
     */
    Collection<Type> all() {
        final Map<String, Type> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Type> member : this.told.entrySet()) {
            found.putIfAbsent(
                this.behaves.getOrDefault(member.getKey(), member.getKey()),
                member.getValue()
            );
        }
        return found.values();
    }
}
