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
 * <p>A row is keyed by the locator of an object and holds a type. There is
 * one form of type so far, an object being a copy of another one, and it is
 * written as an element rather than as a cell of the row:</p>
 *
 * <pre> &lt;type id="Φ.app.φ"&gt;
 *   &lt;ref loc="Φ.app.inc"/&gt;
 * &lt;/type&gt;</pre>
 *
 * <p>A cell would have been shorter and is what this table used to write. It
 * cannot survive the other forms, though: an object is sometimes a datum,
 * sometimes a termination, sometimes a choice between several objects, and a
 * choice is not one answer. Since every one of those wants an element of its
 * own, the one form there is today gets an element too, and the four that
 * follow are added beside it rather than by rewriting this.</p>
 *
 * <p>The rows come out in the order they were worked out, so that two builds
 * of the same program can be compared as text.</p>
 *
 * @since 0.69.0
 */
final class Types {

    /**
     * The pairs, each object against the one it is a copy of.
     */
    private final Map<String, String> copies;

    /**
     * Ctor.
     * @param pairs The pairs, each object against the one it is a copy of
     */
    Types(final Map<String, String> pairs) {
        this.copies = pairs;
    }

    /**
     * This table as an XML document.
     * @return The document
     */
    XML asXml() {
        final Directives dirs = new Directives().add("links");
        for (final Map.Entry<String, String> pair : this.copies.entrySet()) {
            dirs.add("type")
                .attr("id", pair.getKey())
                .add("ref")
                .attr("loc", pair.getValue())
                .up()
                .up();
        }
        return new XMLDocument(new Xembler(dirs).domQuietly());
    }
}
