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
 * Every chain of copies, walked to its end.
 *
 * <p>A copy of a copy is a copy of the same thing, and the pairs come in one
 * at a time: {@code a} is a copy of {@code b}, {@code b} of {@code c}. Asking
 * the table about {@code a} has to arrive at {@code c}, so each pair is
 * followed as far as it goes and the end is written down against every name on
 * the way. A chain that comes back on itself is a ring of copies of one and
 * the same thing, so every name on that ring answers with the same one - the
 * first of them in alphabetical order - no matter which of them is asked.</p>
 *
 * @since 0.68.0
 */
final class Ends {

    /**
     * The pairs, each name against the one it is a copy of.
     */
    private final Map<String, String> copies;

    /**
     * Ctor.
     * @param pairs The pairs, each name against the one it is a copy of
     */
    Ends(final Map<String, String> pairs) {
        this.copies = pairs;
    }

    /**
     * The name every type goes by.
     * @return The names, each type against the end of its chain of copies
     */
    Map<String, String> names() {
        final Map<String, String> ends = new HashMap<>(this.copies.size());
        for (final Map.Entry<String, String> link : this.copies.entrySet()) {
            final Collection<String> walked = new HashSet<>(0);
            walked.add(link.getKey());
            String end = link.getValue();
            while (this.copies.containsKey(end) && walked.add(end)) {
                end = this.copies.get(end);
            }
            if (this.copies.containsKey(end)) {
                end = this.anchor(end);
            }
            ends.put(link.getKey(), end);
        }
        return ends;
    }

    /**
     * The name a ring of copies goes by - the first of its names in
     * alphabetical order, which is the same one wherever the ring is
     * entered.
     * @param node A name on the ring, the one the walk came back to
     * @return The name the whole ring answers with
     */
    private String anchor(final String node) {
        String chosen = node;
        String next = this.copies.get(node);
        while (!next.equals(node)) {
            if (next.compareTo(chosen) < 0) {
                chosen = next;
            }
            next = this.copies.get(next);
        }
        return chosen;
    }
}
