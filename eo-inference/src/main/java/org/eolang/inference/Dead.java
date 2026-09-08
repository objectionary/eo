/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

/**
 * Every object of a program that terminates.
 *
 * <p>The table says which ones do so outright, and a name taken off one of
 * them terminates as well: an object that never comes back with a value has
 * no attribute to hand over, and asking it for one lands where the object
 * itself does. {@link Provided} keeps no row for a termination and would
 * answer such a dispatch with nothing, which is a different fact — nothing
 * known, rather than an answer that fits anywhere.</p>
 *
 * <p>A dispatch found this way carries its receiver's fate on to whoever
 * takes a name off <em>it</em>, so the walk is repeated until it stops
 * growing rather than made once in the order the dispatches happen to be
 * written in.</p>
 *
 * @since 0.71.0
 */
final class Dead {

    /**
     * What the links table says.
     */
    private final Pairs table;

    /**
     * Every dispatch of the program.
     */
    private final Collection<Site> all;

    /**
     * The name every object goes by, once its chain of copies is walked.
     */
    private final Map<String, String> names;

    /**
     * Ctor.
     * @param links What the links table says
     * @param dispatches Every dispatch of the program
     * @param ends The name every object goes by
     */
    Dead(final Pairs links, final Collection<Site> dispatches, final Map<String, String> ends) {
        this.table = links;
        this.all = dispatches;
        this.names = ends;
    }

    /**
     * The locators of everything that terminates.
     * @return The locators, the dispatches on a termination among them
     */
    Collection<String> all() {
        final Collection<String> found = new HashSet<>(0);
        for (final Map.Entry<String, String> row : this.table.forms().entrySet()) {
            if ("terminator".equals(row.getValue())) {
                found.add(row.getKey());
            }
        }
        boolean grown = true;
        while (grown) {
            grown = false;
            for (final Site dispatch : this.all) {
                final String made = dispatch.made();
                final String bearer = dispatch.bearer();
                if (!found.contains(made)
                    && found.contains(this.names.getOrDefault(bearer, bearer))) {
                    found.add(made);
                    grown = true;
                }
            }
        }
        return found;
    }
}
