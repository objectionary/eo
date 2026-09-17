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
 * @since 0.69.0
 */
final class Types {

    /**
     * What every object turns out to be, by its locator.
     */
    private final Map<String, Type> rows;

    /**
     * Ctor.
     *
     * @param types What every object turns out to be, by its locator
     */
    Types(final Map<String, Type> types) {
        this.rows = types;
    }

    /**
     * This table as an XML document.
     *
     * @return The document
     */
    XML asXml() {
        final Directives dirs = new Directives().add("links");
        for (final Map.Entry<String, Type> row : this.rows.entrySet()) {
            dirs.add("type")
                .attr("id", row.getKey())
                .append(row.getValue().directives())
                .up();
        }
        return new XMLDocument(new Xembler(dirs).domQuietly());
    }
}
