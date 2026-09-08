/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The rows of a table, as the document holds them.
 *
 * <p>Every table a clue writes is one element per type under one root, so
 * finding the rows is the same walk whichever table it is and whoever is
 * reading. It is a walk and not a question, since a question about a document
 * of megabytes costs what the whole document costs, and every reader here wants
 * all the rows anyway.</p>
 *
 * @since 0.71.0
 */
final class Rows {

    /**
     * The table.
     */
    private final XML table;

    /**
     * Ctor.
     * @param document The table, as a clue wrote it
     */
    Rows(final XML document) {
        this.table = document;
    }

    /**
     * Every row of the table.
     * @return The rows, in the order the document holds them
     */
    List<Xnav> all() {
        return new Xnav(this.table.inner())
            .elements()
            .flatMap(root -> root.elements(Filter.withName("type")))
            .collect(Collectors.toList());
    }
}
