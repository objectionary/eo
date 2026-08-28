/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * What every void of a program is seen to be filled with, as locators.
 *
 * <p>{@link Fillings} asks the same table the same question and comes back
 * with types, which is what a reader of the tables wants. {@link Relayed}
 * wants the objects themselves, since it has to look into the one that fills
 * a void and count the voids <em>it</em> declares, and a type says nothing
 * about those.</p>
 *
 * @since 0.70.0
 */
final class Fillers {

    /**
     * The links table.
     */
    private final XML table;

    /**
     * Ctor.
     * @param links The links table, as {@link Resolved} left it
     */
    Fillers(final XML links) {
        this.table = links;
    }

    /**
     * What goes into every void.
     * @return The locators of what goes in, by the locator of the void,
     *  without the voids nobody ever fills
     */
    Map<String, Collection<String>> all() {
        final Map<String, Collection<String>> found = new LinkedHashMap<>(0);
        for (final XML bind : this.table.nodes("/links/type/ref/bind[ref]")) {
            found.computeIfAbsent(
                bind.xpath("@void").get(0), key -> new LinkedHashSet<>(0)
            ).add(bind.xpath("ref/@loc").get(0));
        }
        return found;
    }
}
