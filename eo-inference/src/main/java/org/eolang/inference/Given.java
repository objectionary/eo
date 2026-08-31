/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * What every application puts into the voids of what it copies.
 *
 * <p>An argument goes into a void by its place and not by its name, and the
 * place is what {@code @as} says, not where the argument sits among its
 * siblings: an inline binding such as {@code pair 5:1} names its void
 * directly and can leave an earlier one for whoever applies next. A binding
 * may name the void itself rather than its place, and is kept apart for that
 * reason (R-3.12.1). What the argument is stays a locator here, since the
 * type of it is a question for the links table and the answer changes as the
 * passes go on.</p>
 *
 * @since 0.69.0
 */
final class Given {

    /**
     * Every application of the program.
     */
    private final Collection<XML> all;

    /**
     * Ctor.
     * @param applications Every application of the program
     */
    Given(final Collection<XML> applications) {
        this.all = applications;
    }

    /**
     * The arguments of every application, by the locator of the application.
     * @return The arguments, each list in the order the places run, with an
     *  empty locator where an inline binding has left a place unfilled
     */
    Map<String, List<String>> arguments() {
        final Map<String, List<String>> found = new HashMap<>(0);
        for (final XML application : this.all) {
            found.put(application.xpath("@loc").get(0), new Placed(application).args());
        }
        return found;
    }

    /**
     * The arguments of every application bound by name, by the locator of the
     * application.
     * @return The arguments, by the name of the void each names
     */
    Map<String, Map<String, String>> named() {
        final Map<String, Map<String, String>> found = new HashMap<>(0);
        for (final XML application : this.all) {
            final Map<String, String> args = new HashMap<>(0);
            for (final XML arg : application.nodes(
                "o[@as][not(starts-with(@as, 'α'))][@loc]"
            )) {
                args.put(arg.xpath("@as").get(0), arg.xpath("@loc").get(0));
            }
            found.put(application.xpath("@loc").get(0), args);
        }
        return found;
    }
}
