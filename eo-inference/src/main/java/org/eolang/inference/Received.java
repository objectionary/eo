/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.util.Collection;
import java.util.HashSet;

/**
 * What the receiver of an attribute holds.
 *
 * <p>An attribute of an {@code oak} is reached by taking it off an
 * {@code oak}, and there is no other way to reach it: a dispatch that lands on
 * {@code Φ.oak.grow} landed there because what it was taken off was an
 * {@code oak}, and one that goes through a decorator arrives no differently,
 * since the lookup is handed down and the decoratee is the {@code oak}. So the
 * void a formation declares for its receiver holds the object the formation is
 * an attribute of, which is what {@link Held} reads back and {@link Provided}
 * walks through.</p>
 *
 * <p>This is not the old rule of reading a receiver off a locator, which was
 * taken out again in #6657 and should stay out. That one gave every object a
 * {@code ρ} whether it had asked for one or not, and a formation that declares
 * no receiver has none at all — the runtime terminates on the {@code ρ} of
 * such an object. Nothing is invented here: the row is in the table already,
 * written because the source declared the void, and only what goes into it is
 * added.</p>
 *
 * <p>Nothing is said where the locator above the formation names no object of
 * the program. A package is a prefix and nothing else until a file forms
 * something of that name, so a {@code Φ.sys.posix.stdout} sits under a
 * {@code Φ.sys.posix} nobody wrote and there is no type for its receiver to
 * hold. What a void says for itself is left alone as well, an atom that
 * annotates its own {@code ^} knowing better than a locator does.</p>
 *
 * @since 0.71.0
 */
final class Received {

    /**
     * The formations of the program.
     */
    private final Collection<XML> made;

    /**
     * Ctor.
     *
     * @param formations The formations of the program
     */
    Received(final Collection<XML> formations) {
        this.made = formations;
    }

    /**
     * Write what every declared receiver holds into the given table.
     *
     * @param rows The table to fill
     */
    void fill(final Tojos rows) {
        final Collection<String> owners = new HashSet<>(0);
        for (final XML formation : this.made) {
            owners.add(new Noted(formation).says("loc"));
        }
        for (final XML formation : this.made) {
            final String owner = new Noted(formation).says("loc");
            final String above = Received.above(owner);
            if (owners.contains(above) && Received.receives(formation)) {
                final Tojo row = rows.add(String.join(" ", owner, "ρ"));
                if (!row.exists("holds")) {
                    row.set("holds", above);
                }
            }
        }
    }

    private static String above(final String owner) {
        String found = "";
        final int last = owner.lastIndexOf('.');
        if (last > 0) {
            found = owner.substring(0, last);
        }
        return found;
    }

    private static boolean receives(final XML formation) {
        return new Xnav(formation.inner())
            .elements(Filter.withName("o"))
            .map(Noted::new)
            .anyMatch(kid -> "∅".equals(kid.says("base")) && "ρ".equals(kid.says("name")));
    }
}
