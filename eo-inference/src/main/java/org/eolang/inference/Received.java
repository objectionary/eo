/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import com.yegor256.tojos.Tojos;
import java.util.Collection;
import java.util.stream.Collectors;
import org.xembly.Directives;

/**
 * What the receiver of an attribute holds.
 *
 * <p>An attribute of an {@code oak} is reached by taking it off an
 * {@code oak}, and there is no other way to reach it: a dispatch that lands on
 * {@code Φ.oak.grow} landed there because what it was taken off was an
 * {@code oak}, and one that goes through a decorator arrives no differently,
 * since the lookup is handed down and the decoratee is the {@code oak}. So the
 * void a formation declares for its receiver holds the object the formation is
 * an attribute of, which is what {@code Held} reads back and {@code Provided}
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
 * <p>An object at the top of a file has a receiver too, though no formation
 * of the program is around it: the runtime puts into its {@code ^} the
 * package it was loaded from, so the {@code ^} of {@code Φ.true} holds
 * {@code Φ} and the {@code ^} of a {@code Φ.io.stdout} with no {@code io.eo}
 * holds the package {@code Φ.io}. A package is a row of the table like a
 * formation is, {@link Packages} having written it already, so the rows
 * the table has for whole objects are exactly what a receiver can hold, and
 * they are read from there rather than worked out again.</p>
 *
 * <p>What is written is the {@code rho} of the void, the formation a declared
 * {@code ^} sits in, and never what it {@code holds}: the latter is what the
 * source says a void will hold, the former is worked out from where the
 * formation sits, and the two are kept apart so that a reader can tell them
 * apart. A void that says both keeps
 * both, and whoever reads them takes what the void says first, an atom that
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
     * The formation every declared {@code ^} sits in, written into the
     * objects as the {@code rho} of the void.
     *
     * @param rows The table, with every formation and every package in it
     *  already
     * @return The directives, which start and end where the {@code objects}
     *  element is a child of the cursor
     */
    Directives directives(final Tojos rows) {
        final Collection<String> owners = rows.select(one -> !one.exists("owner"))
            .stream()
            .map(one -> one.get("id"))
            .collect(Collectors.toSet());
        final Directives dirs = new Directives();
        for (final XML formation : this.made) {
            final String owner = new Noted(formation).says("loc");
            final String above = Received.above(owner);
            if (owners.contains(above) && Received.receives(formation)) {
                dirs.xpath(String.format("objects/type[@id='%s']/attr[@name='ρ']", owner))
                    .attr("rho", above)
                    .up()
                    .up()
                    .up();
            }
        }
        return dirs;
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
