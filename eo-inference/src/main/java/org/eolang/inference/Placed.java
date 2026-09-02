/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * The arguments of one application, ordered by the place each one names.
 *
 * <p>An inline binding {@code :N} says which place the argument goes to, so
 * the order the arguments are written in is not the order they fill. A place
 * nobody names comes back as an empty locator, since the list is read by
 * position and a gap has to keep its own seat.</p>
 *
 * @since 0.69.0
 */
final class Placed {

    /**
     * The objects bound inside the application.
     */
    private final Collection<Xnav> bound;

    /**
     * Ctor.
     * @param objects The objects bound inside the application
     */
    Placed(final Collection<Xnav> objects) {
        this.bound = objects;
    }

    /**
     * The arguments, one per place, up to the highest place named.
     * @return The locator of every argument, by its place
     */
    List<String> args() {
        final Map<Integer, String> byplace = new HashMap<>(1);
        int highest = -1;
        for (final Xnav arg : this.bound) {
            final Optional<String> place = arg.attribute("as").text();
            final Optional<String> loc = arg.attribute("loc").text();
            if (place.isPresent() && loc.isPresent() && place.get().startsWith("α")) {
                final int index = Integer.parseInt(place.get().substring(1));
                byplace.put(index, loc.get());
                highest = Math.max(highest, index);
            }
        }
        final List<String> args = new ArrayList<>(highest + 1);
        for (int place = 0; place <= highest; place += 1) {
            args.add(byplace.getOrDefault(place, ""));
        }
        return args;
    }
}
