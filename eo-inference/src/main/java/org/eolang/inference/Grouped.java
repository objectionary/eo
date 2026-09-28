/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Tojo;
import com.yegor256.tojos.Tojos;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * A table of types and their attributes, as a document a human can read.
 *
 * <p>The rows of a table are flat, because that is what a row is: a few
 * named cells. A row about a type is written out as it stands, its
 * {@code id} being the name of the type itself. A row about an attribute
 * keeps two cells that say nothing about the attribute and only place the
 * row in the table: its {@code id}, which is bookkeeping (the owner and
 * the name, since neither alone is unique), and its {@code owner}, which
 * the document says by nesting instead. Those cells are dropped, and every
 * other one becomes an attribute of its own, whatever it happens to be
 * called:</p>
 *
 * <pre> &lt;provides&gt;
 *   &lt;type id="Φ.t" complete="true"&gt;
 *     &lt;attr name="next" type="Φ.t.next"/&gt;
 *   &lt;/type&gt;
 * &lt;/provides&gt;</pre>
 *
 * <p>Knowing no other column by name is what lets one view serve more
 * than one table: the needs table has the same shape as this one — an
 * owner, an attribute and its type — and will be rendered by the same
 * class, while a rule that starts recording something new about a type
 * gets it into the document for free.</p>
 *
 * @since 0.67.0
 */
final class Grouped {

    /**
     * The rows.
     */
    private final Tojos table;

    /**
     * The name of the table.
     */
    private final String root;

    /**
     * Ctor.
     *
     * @param rows The rows to render
     * @param name The name of the table, which the document is named after
     */
    Grouped(final Tojos rows, final String name) {
        this.table = rows;
        this.root = name;
    }

    /**
     * This table as an XML document.
     *
     * @return The document
     */
    XML asXml() {
        final Map<String, List<Map<String, String>>> attrs = this.attributes();
        final Directives dirs = new Directives().add(this.root);
        for (final Tojo type : this.types()) {
            final Map<String, String> cells = type.toMap();
            dirs.add("type").append(new Cells(cells).directives());
            for (final Map<String, String> attr
                : attrs.getOrDefault(cells.get("id"), Collections.emptyList())) {
                dirs.add("attr")
                    .append(new Cells(attr, Arrays.asList("id", "owner")).directives())
                    .up();
            }
            dirs.up();
        }
        return new XMLDocument(new Xembler(dirs).domQuietly());
    }

    private List<Tojo> types() {
        return this.table.select(row -> !row.exists("owner"));
    }

    private Map<String, List<Map<String, String>>> attributes() {
        final Map<String, List<Map<String, String>>> gathered = new LinkedHashMap<>(0);
        for (final Tojo row : this.table.select(one -> one.exists("owner"))) {
            gathered.computeIfAbsent(
                row.get("owner"), key -> new ArrayList<>(1)
            ).add(row.toMap());
        }
        return gathered;
    }
}
