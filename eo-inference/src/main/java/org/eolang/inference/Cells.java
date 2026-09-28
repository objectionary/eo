/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import org.xembly.Directives;

/**
 * The cells of one row, as attributes of an element.
 *
 * <p>A row is a handful of named cells, and most of them are facts worth
 * reporting: whatever the rule chose to call them, they become attributes
 * of the same name. A few say nothing about the type and only place the
 * row in its table — the identifier of the row, the owner it belongs to —
 * and the document says those by nesting instead, so they are named here
 * and left out.</p>
 *
 * @since 0.67.0
 */
final class Cells {

    /**
     * The row.
     */
    private final Map<String, String> row;

    /**
     * The names of the cells that only place the row.
     */
    private final Collection<String> placement;

    /**
     * Ctor.
     *
     * @param cells The row, every cell of which is a fact
     */
    Cells(final Map<String, String> cells) {
        this(cells, Collections.emptyList());
    }

    /**
     * Ctor.
     *
     * @param cells The row
     * @param skipped The names of the cells that only place the row
     */
    Cells(final Map<String, String> cells, final Collection<String> skipped) {
        this.row = cells;
        this.placement = skipped;
    }

    /**
     * These cells as attributes of the element being built.
     *
     * @return The directives
     */
    Directives directives() {
        final Directives dirs = new Directives();
        for (final Map.Entry<String, String> cell : this.row.entrySet()) {
            if (!this.placement.contains(cell.getKey())) {
                dirs.attr(cell.getKey(), cell.getValue());
            }
        }
        return dirs;
    }
}
