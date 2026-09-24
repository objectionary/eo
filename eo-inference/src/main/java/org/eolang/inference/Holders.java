/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

/**
 * Which objects each void of a program turns out to hold.
 *
 * <p>A void is filled by a caller, and a void of a library object is filled by
 * every caller there is: the {@code if} of {@code Φ.bool} holds the formation
 * that hands back its left arm, from {@code Φ.true}, and the one that hands
 * back its right arm, from {@code Φ.false}. Both of them, since a question
 * about a boolean is answered by whichever of the two the boolean turns out to
 * be.</p>
 *
 * <p>{@link Bound} says what each call filled, keyed by the call. This says the
 * same thing keyed by the void, which is what a reader of the void wants to
 * know and would otherwise have to look for through every call of the
 * program.</p>
 *
 * @since 0.71.0
 */
final class Holders {

    /**
     * What every application fills, from {@link Bound}.
     */
    private final Map<String, Map<String, String>> fills;

    /**
     * The pairs, each name against the one it is a copy of.
     */
    private final Map<String, String> pairs;

    /**
     * Ctor.
     *
     * @param bound What every application fills, from {@link Bound}
     * @param links The pairs, each name against the one it is a copy of
     */
    Holders(final Map<String, Map<String, String>> bound, final Map<String, String> links) {
        this.fills = bound;
        this.pairs = links;
    }

    /**
     * What every void holds, by the locator of the void.
     *
     * @return The objects put into a void by any call, by the locator of the
     *  void, each object under the name it ends up going by
     */
    Map<String, Collection<String>> all() {
        final Map<String, Collection<String>> found = new HashMap<>(0);
        final Ends ends = new Ends(this.pairs);
        for (final Map<String, String> filled : this.fills.values()) {
            for (final Map.Entry<String, String> fill : filled.entrySet()) {
                found.computeIfAbsent(fill.getKey(), hollow -> new HashSet<>(0))
                    .add(ends.name(fill.getValue()));
            }
        }
        return found;
    }
}
