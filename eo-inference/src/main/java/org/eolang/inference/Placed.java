/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
     * The application.
     */
    private final XML application;

    /**
     * Ctor.
     * @param app The application
     */
    Placed(final XML app) {
        this.application = app;
    }

    /**
     * The arguments, one per place, up to the highest place named.
     * @return The locator of every argument, by its place
     */
    List<String> args() {
        final Map<Integer, String> byplace = new HashMap<>(1);
        int highest = -1;
        for (final XML arg : this.application.nodes("o[starts-with(@as, 'α')][@loc]")) {
            final int place = Integer.parseInt(arg.xpath("@as").get(0).substring(1));
            byplace.put(place, arg.xpath("@loc").get(0));
            highest = Math.max(highest, place);
        }
        final List<String> args = new ArrayList<>(highest + 1);
        for (int place = 0; place <= highest; place += 1) {
            args.add(byplace.getOrDefault(place, ""));
        }
        return args;
    }
}
