/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import java.util.LinkedHashMap;
import java.util.Map;
import org.w3c.dom.NamedNodeMap;

/**
 * One row of a table, as its document keeps it.
 *
 * <p>A row is a few named cells and nothing else, which is what
 * {@link Cells} turns into a document and what this turns back. No column is
 * known here by name, so a rule that starts writing a new one has it read
 * back without anything being changed.</p>
 *
 * @since 0.68.0
 */
final class Row {

    /**
     * The element of the document the row was written as.
     */
    private final Xnav element;

    /**
     * Ctor.
     * @param row The element of the document the row was written as
     */
    Row(final Xnav row) {
        this.element = row;
    }

    /**
     * The cells of the row.
     * @return The cells, by their names, in the order the document keeps them
     */
    Map<String, String> cells() {
        final Map<String, String> found = new LinkedHashMap<>(0);
        final NamedNodeMap named = this.element.node().getAttributes();
        for (int cell = 0; cell < named.getLength(); cell = cell + 1) {
            found.put(named.item(cell).getNodeName(), named.item(cell).getNodeValue());
        }
        return found;
    }
}
