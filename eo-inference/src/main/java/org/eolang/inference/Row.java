/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * One row of a table: a few named cells.
 *
 * <p>A row says one thing about one object, and what it says is whatever
 * cells the rule chose to write. Only {@code id} is agreed upon, since two
 * rows about the same thing would be a contradiction, and {@code owner},
 * which marks a row as being about an attribute of something rather than
 * about a type. Everything else is the rule's own business, and reaches the
 * document unread — a rule that starts recording something new gets it into
 * the table for free.</p>
 *
 * <p>A table is a list, and the list is kept in the order the rows were
 * written, which is the order of the code. That order is not decoration: an
 * application binds its arguments to the voids of a formation in the order
 * they were declared, so the rule that checks applications will ask for the
 * first void and must get the same answer every time.</p>
 *
 * @since 0.68.0
 */
final class Row {

    /**
     * The cells, by name, in the order they were written.
     */
    private final Map<String, String> cells;

    /**
     * Ctor.
     * @param id What this row is about
     */
    Row(final String id) {
        this(Collections.singletonMap("id", id));
    }

    /**
     * Ctor.
     * @param named The cells, by name
     */
    private Row(final Map<String, String> named) {
        this.cells = named;
    }

    /**
     * This row with one more cell in it.
     * @param name The name of the cell
     * @param value What it says
     * @return A row of its own
     */
    Row with(final String name, final String value) {
        final Map<String, String> more = new LinkedHashMap<>(this.cells);
        more.put(name, value);
        return new Row(more);
    }

    /**
     * Whether this row has such a cell.
     * @param name The name of the cell
     * @return True if it does
     */
    boolean has(final String name) {
        return this.cells.containsKey(name);
    }

    /**
     * What such a cell says.
     * @param name The name of the cell
     * @return What it says
     */
    String cell(final String name) {
        return this.cells.get(name);
    }

    /**
     * Every cell of this row.
     * @return The cells, by name, in the order they were written
     */
    Map<String, String> all() {
        return Collections.unmodifiableMap(this.cells);
    }
}
