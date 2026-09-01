/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The rows of a table, read back from the document {@link Grouped} wrote.
 *
 * <p>A clue writes a table for a human to read, nested and short; the loop
 * needs the same table flat and by the name of its owner, since it asks about
 * tens of thousands of types and searching a document for every one of them
 * costs more than the whole checking does. This turns the one into the other:
 * a type row and the rows of its attributes end up together, under the name
 * that the type and all its copies go by.</p>
 *
 * <p>Which of the two a row is can be seen from its cells, exactly as
 * {@link Grouped} decided when it wrote them: a type row carries an
 * {@code id}, a row about an attribute carries a {@code name}. Nothing else is
 * known about the columns here, so a rule that starts writing a new one has it
 * read back for free.</p>
 *
 * @since 0.68.0
 */
final class Ungrouped {

    /**
     * The table.
     */
    private final XML table;

    /**
     * The name every type goes by.
     */
    private final Map<String, String> names;

    /**
     * Ctor.
     * @param document The table, as a clue wrote it
     * @param aliases The name every type goes by, from {@link Same}
     */
    Ungrouped(final XML document, final Map<String, String> aliases) {
        this.table = document;
        this.names = aliases;
    }

    /**
     * The rows of the table, by the name their owner goes by.
     * @return The rows, in the order the table keeps them, since the place of
     *  a row is what says which void an argument fills
     */
    Map<String, Collection<Map<String, String>>> rows() {
        final Map<String, Collection<Map<String, String>>> found = new LinkedHashMap<>(0);
        for (final Xnav type : new Rows(this.table).all()) {
            final Map<String, String> cells = new Row(type).cells();
            final Collection<Map<String, String>> owned = found.computeIfAbsent(
                this.names.getOrDefault(cells.get("id"), cells.get("id")),
                key -> new ArrayList<>(1)
            );
            owned.add(cells);
            type.elements(Filter.withName("attr")).forEach(
                attr -> owned.add(new Row(attr).cells())
            );
        }
        return found;
    }
}
