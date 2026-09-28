/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

/**
 * A chain of links, followed to where it stops.
 *
 * <p>One object is a copy of another and that one of a third, and the walk
 * takes every hop it is given until it is given none. A chain that comes back
 * on itself stops at the object it comes back to, since an object that is a
 * copy of itself is nothing new.</p>
 *
 * <p>The last hop is of a different kind. Where the walk stops at an atom, the
 * caller was handed what the atom comes back with and not the atom, so the
 * forma it declares is the answer and the atom is not.</p>
 *
 * @since 0.69.0
 */
final class Walked {

    /**
     * What every object is a copy of.
     */
    private final Map<String, String> hops;

    /**
     * What every atom comes back with.
     */
    private final Map<String, String> backs;

    /**
     * Ctor.
     *
     * @param copies What every object is a copy of, from {@link Pairs}
     * @param returns What every atom comes back with
     */
    Walked(final Map<String, String> copies, final Map<String, String> returns) {
        this.hops = copies;
        this.backs = returns;
    }

    /**
     * Where the chain from this object stops.
     *
     * @param start The locator to walk from
     * @return The locator the walk arrives at
     */
    String from(final String start) {
        final Collection<String> walked = new HashSet<>(0);
        String where = start;
        while (walked.add(where) && this.hops.containsKey(where)) {
            where = this.hops.get(where);
        }
        return this.backs.getOrDefault(where, where);
    }
}
