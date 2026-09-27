/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The voids the program fills one way, each against the object it holds.
 *
 * <p>{@link Witnessed} writes down what every caller was seen putting into
 * every void, {@link Seen} reads it back and {@link Sole} says which of those
 * censuses have an answer in them; this is the three of them wired up, so that
 * whoever has the provides table and wants the voids that are worth naming says
 * so in one line. 1,187 of the 2,026 voids of eo-runtime hold one thing and
 * nothing else, and 1,087 of those hold something the table has a row for.</p>
 *
 * @since 0.74.0
 */
final class Ones {

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * Ctor.
     *
     * @param provides The provides table, with the census {@link Witnessed}
     *  has written into it
     */
    Ones(final XML provides) {
        this.given = provides;
    }

    /**
     * Every void that holds one object and nothing else.
     *
     * @return The locator of what it holds, by the locator of the void
     */
    Map<String, String> all() {
        final Collection<String> known = this.known();
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Collection<Type>> hollow
            : new Seen(this.given).all().entrySet()) {
            final String sole = new Sole(hollow.getValue(), known).names();
            if (!sole.isEmpty()) {
                found.put(hollow.getKey(), sole);
            }
        }
        return found;
    }

    private Collection<String> known() {
        final Collection<String> found = new HashSet<>(0);
        for (final Xnav type : new Rows(this.given).all()) {
            found.add(new Noted(type).says("id"));
        }
        return found;
    }
}
