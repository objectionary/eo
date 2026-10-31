/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.util.Map;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * What every object of a program turns out to be, as a document.
 *
 * <p>A row is keyed by the locator of an object and holds one {@link Type},
 * written as an element rather than as a cell of the row:</p>
 *
 * <pre> &lt;type id="Φ.app.held"&gt;
 *   &lt;ref loc="Φ.inc"&gt;
 *     &lt;bind void="Φ.inc.x"&gt;
 *       &lt;ref loc="Φ.app.held.α0"/&gt;
 *     &lt;/bind&gt;
 *   &lt;/ref&gt;
 * &lt;/type&gt;
 * &lt;type id="Φ.app.held.α0.α0"&gt;
 *   &lt;data/&gt;
 * &lt;/type&gt;</pre>
 *
 * <p>A cell would have been shorter and is what this table used to write. It
 * cannot survive the other forms, though: an object is sometimes a datum,
 * sometimes a termination, sometimes a choice between several objects, and a
 * choice is not one answer. Nor can a collection per form, which is what
 * replaced it: three of them for three forms, and four more forms coming.
 * A row holds one type, and so does this.</p>
 *
 * <p>The rows come out in the order they were worked out, so that two builds
 * of the same program can be compared as text.</p>
 *
 * <p>The same rows are a table of their own once the links are worked out,
 * and a section of the facts before that: what {@link Starts} reads off the
 * heads, the data and the terminations is stated by the text, and
 * goes under {@code heads} in {@code facts.xml}. So the element that holds
 * the rows is named by whoever writes them.</p>
 *
 * @since 0.69.0
 */
final class Types {

    /**
     * What every object turns out to be, by its locator.
     */
    private final Map<String, Type> rows;

    /**
     * The name of the element that holds the rows.
     */
    private final String root;

    /**
     * Ctor.
     *
     * @param types What every object turns out to be, by its locator
     */
    Types(final Map<String, Type> types) {
        this(types, "links");
    }

    /**
     * Ctor.
     *
     * @param types What every object turns out to be, by its locator
     * @param name The name of the element that holds the rows
     */
    Types(final Map<String, Type> types, final String name) {
        this.rows = types;
        this.root = name;
    }

    /**
     * This table as an XML document.
     *
     * @return The document
     */
    XML asXml() {
        return new XMLDocument(new Xembler(this.directives()).domQuietly());
    }

    /**
     * This table as directives, which leave the cursor where they found it.
     *
     * @return The directives
     */
    Directives directives() {
        final Directives dirs = new Directives().add(this.root);
        for (final Map.Entry<String, Type> row : this.rows.entrySet()) {
            dirs.add("type")
                .attr("id", row.getKey())
                .append(row.getValue().directives())
                .up();
        }
        return dirs.up();
    }
}
