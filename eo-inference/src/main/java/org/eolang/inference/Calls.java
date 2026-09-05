/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Every call the program makes, against the object it was made on.
 *
 * <p>An application names no receiver of its own — it copies whatever its base
 * settled on — so the links are what say who was called. A call written as
 * {@code ^.body index} arrives at the {@code body} of a {@code while}, and it
 * is only after the walk that the object called turns out to be a void.</p>
 *
 * <p>What was passed is settled the same way {@link Fillings} settles a
 * filling, and for the same reason: an argument written at one call site is a
 * locator nothing joins to the argument written at the next, and the two are
 * one type. A walk that runs into a void has no type to give, so what is kept
 * is the void it stopped at, which is a fact about another caller and still a
 * fact.</p>
 *
 * @since 0.72.0
 */
final class Calls {

    /**
     * Every application of the program.
     */
    private final Collection<XML> sites;

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
     * @param applications Every application of the program
     * @param table What the links table says, as {@link Resolved} left it
     * @param provides The provides table, which says where an argument can
     *  land
     */
    Calls(final Collection<XML> applications, final Pairs table, final XML provides) {
        this.sites = applications;
        this.links = table;
        this.given = provides;
    }

    /**
     * Every call, one per argument.
     * @return The calls, without the ones that pass the same type at the same
     *  place of the same object twice
     */
    Collection<Call> all() {
        final Map<String, String> names = new Ends(this.links.all()).names();
        final Map<String, String> landings = new Landed(this.links, this.given).all();
        final Forms forms = new Forms(this.links.forms());
        final Map<String, Call> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, List<String>> made
            : new Given(this.sites).arguments().entrySet()) {
            final String applied = names.getOrDefault(made.getKey(), made.getKey());
            final List<String> args = made.getValue();
            for (int place = 0; place < args.size(); place += 1) {
                final String arg = args.get(place);
                if (!arg.isEmpty()) {
                    final String end = landings.getOrDefault(arg, names.getOrDefault(arg, arg));
                    found.putIfAbsent(
                        String.join(" ", applied, String.valueOf(place), forms.name(end)),
                        new Call(applied, place, forms.type(end))
                    );
                }
            }
        }
        return found.values();
    }
}
