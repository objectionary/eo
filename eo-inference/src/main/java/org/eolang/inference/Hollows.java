/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.LinkedHashSet;

/**
 * The locator of every void of a program.
 *
 * <p>Half the rules ask whether a name belongs to a void, and none of them
 * ask which void is which: {@link Rooted} walks a locator up until one of
 * these answers, {@link Provided} stops a walk that arrives at one. So what
 * they want is a set, and being handed a list instead makes every one of those
 * questions a walk of the whole of it — on {@code eo-runtime} that is 1,593
 * comparisons per question, asked of 13,325 dispatches on every pass of a
 * fixpoint that runs more than a hundred of them.</p>
 *
 * <p>The order the table wrote them in is kept all the same, since a table
 * read back the same way twice is a table a reader can diff.</p>
 *
 * @since 0.73.0
 */
final class Hollows {

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * Ctor.
     *
     * @param provides The provides table, as {@link Provides} wrote it
     */
    Hollows(final XML provides) {
        this.given = provides;
    }

    /**
     * The locator of every void the table names.
     *
     * @return The locators, in the order the table names them
     */
    Collection<String> all() {
        final Collection<String> found = new LinkedHashSet<>(0);
        for (final Xnav type : new Rows(this.given).all()) {
            type.elements(Filter.withName("attr"))
                .filter(attr -> "true".equals(new Noted(attr).says("void")))
                .forEach(attr -> found.add(new Noted(attr).says("type")));
        }
        return found;
    }
}
