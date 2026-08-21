/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * What every application puts into the voids of what it copies.
 *
 * <p>An argument goes into a void by its place and not by its name, so the
 * order is the whole of the fact and it is kept. What the argument is stays a
 * locator here, since the type of it is a question for the links table and the
 * answer changes as the passes go on.</p>
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
     * @return The arguments, each list in the order the places run
     */
    Map<String, List<String>> arguments() {
        final Map<String, List<String>> found = new HashMap<>(0);
        for (final XML application : this.all) {
            final List<String> args = new ArrayList<>(1);
            for (final XML arg : application.nodes("o[starts-with(@as, 'α')][@loc]")) {
                args.add(arg.xpath("@loc").get(0));
            }
            found.put(application.xpath("@loc").get(0), args);
        }
        return found;
    }
}
