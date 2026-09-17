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
 * The pairs and the binds, as the rows of a table.
 *
 * <p>Two things are worked out apart and belong together in one row: what an
 * object is a copy of, and which voids of that copy it has filled. Neither is
 * of any use without the other — a copy nobody has given arguments to and a
 * saturated one are the same pair and different types.</p>
 *
 * <p>A third joins them for the few objects that have it: the arms a call on a
 * void may come back with, where the pair alone says no more than which void
 * it was (#8744).</p>
 *
 * @since 0.69.0
 */
final class Refs {

    /**
     * The pairs, each object against the one it is a copy of.
     */
    private final Map<String, String> copies;

    /**
     * What every application put into the voids of what it copies.
     */
    private final Map<String, Map<String, String>> filled;

    /**
     * What every call on a void may come back with, from {@link Dispatched}.
     */
    private final Map<String, Collection<String>> arms;

    /**
     * Ctor.
     *
     * @param pairs The pairs, each object against the one it is a copy of
     * @param binds What every application put into the voids of what it
     *  copies, from {@link Bound}
     * @param chosen What every call on a void may come back with, from
     *  {@link Dispatched}
     */
    Refs(
        final Map<String, String> pairs,
        final Map<String, Map<String, String>> binds,
        final Map<String, Collection<String>> chosen
    ) {
        this.copies = pairs;
        this.filled = binds;
        this.arms = chosen;
    }

    /**
     * These pairs as rows.
     *
     * @return The types, by the locator of the object they are about, in the
     *  order the pairs came in
     */
    Map<String, Type> all() {
        final Map<String, Type> found = new LinkedHashMap<>(this.copies.size());
        for (final Map.Entry<String, String> pair : this.copies.entrySet()) {
            found.put(
                pair.getKey(),
                new Ref(
                    pair.getValue(),
                    this.filled.getOrDefault(pair.getKey(), Collections.emptyMap()),
                    this.arms.getOrDefault(pair.getKey(), Collections.emptyList())
                )
            );
        }
        return found;
    }
}
