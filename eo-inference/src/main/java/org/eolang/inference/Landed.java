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
 * Where a filling lands.
 *
 * <p>{@link Ends} walks a chain of copies and stops as soon as the chain does,
 * which is the right answer to the question it asks and the wrong one here.
 * An argument is written afresh at every call site, so {@code 01-.eq x} passed
 * at eleven places is eleven locators that no chain of copies joins, and
 * counting them apart makes a void look filled eleven ways when it is filled
 * one way eleven times.</p>
 *
 * <p>So the walk goes on through the links until it arrives somewhere the
 * table describes: a formation, a datum, or an object that never comes back.
 * Those eleven all arrive at the same {@code Φ.bytes.eq} and become one.
 * Where the walk arrives at an atom, the answer is what the atom comes back
 * with rather than the atom: a caller handed the result of {@code 01-.eq x},
 * which is a {@code Φ.bool}, and never the {@code eq} itself.</p>
 *
 * <p>A walk that arrives at none of those is not in the answer at all. It ran
 * into a void, and what fills that void is the business of whoever fills it —
 * a fact about another caller, not about this one.</p>
 *
 * @since 0.69.0
 */
final class Landed {

    /**
     * What the links table says.
     */
    private final Pairs links;

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * Ctor.
     * @param table What the links table says, as {@link Resolved} left it
     * @param provides The provides table, which says what an atom comes back
     *  with and which objects are formations
     */
    Landed(final Pairs table, final XML provides) {
        this.links = table;
        this.given = provides;
    }

    /**
     * Where every object the table can place ends up.
     * @return The landings, by the locator of the object, without the objects
     *  whose walk runs into a void
     */
    Map<String, String> all() {
        final Collection<String> made = new HashSet<>(0);
        for (final Xnav type : new Rows(this.given).all()) {
            made.add(new Noted(type).says("id"));
        }
        final Collection<String> plain = new HashSet<>(this.links.certain());
        final Map<String, String> hops = this.links.all();
        final Walked walked = new Walked(hops, new Returned(this.given).all());
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final String type : made) {
            found.put(type, type);
        }
        for (final String ground : plain) {
            found.put(ground, ground);
        }
        for (final String start : hops.keySet()) {
            final String end = walked.from(start);
            if (made.contains(end) || plain.contains(end)) {
                found.put(start, end);
            }
        }
        return found;
    }
}
