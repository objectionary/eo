/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.util.Collections;
import java.util.Map;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * What every object of a program turns out to be, as a document.
 *
 * <p>A row is keyed by the locator of an object and holds a type. There are
 * two forms of type so far — an object being a copy of another one, and the
 * voids of that copy it has filled — and both are written as elements rather
 * than as cells of the row:</p>
 *
 * <pre> &lt;type id="Φ.app.held"&gt;
 *   &lt;ref loc="Φ.inc"&gt;
 *     &lt;bind void="Φ.inc.x"&gt;
 *       &lt;ref loc="Φ.app.held.α0"/&gt;
 *     &lt;/bind&gt;
 *   &lt;/ref&gt;
 * &lt;/type&gt;</pre>
 *
 * <p>A cell would have been shorter and is what this table used to write. It
 * cannot survive the other forms, though: an object is sometimes a datum,
 * sometimes a termination, sometimes a choice between several objects, and a
 * choice is not one answer. Since every one of those wants an element of its
 * own, the one form there was got an element too, and the ones that follow are
 * added beside it rather than by rewriting this.</p>
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
     * What every application put into the voids of what it copies.
     */
    private final Map<String, Map<String, String>> filled;

    /**
     * Ctor.
     * @param pairs The pairs, each object against the one it is a copy of
     */
    Types(final Map<String, String> pairs) {
        this(pairs, Collections.emptyMap());
    }

    /**
     * Ctor.
     * @param pairs The pairs, each object against the one it is a copy of
     * @param binds What every application put into the voids of what it
     *  copies, from {@link Bound}
     */
    Types(final Map<String, String> pairs, final Map<String, Map<String, String>> binds) {
        this.copies = pairs;
        this.filled = binds;
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
                .append(this.binds(pair.getKey()))
                .up()
                .up();
        }
        return new XMLDocument(new Xembler(dirs).domQuietly());
    }

    /**
     * The voids this object has filled.
     * @param id The locator of the object
     * @return The directives that put them inside its type, empty when it
     *  fills none
     */
    private Directives binds(final String id) {
        final Directives dirs = new Directives();
        for (final Map.Entry<String, String> bind
            : this.filled.getOrDefault(id, Collections.emptyMap()).entrySet()) {
            dirs.add("bind")
                .attr("void", bind.getKey())
                .add("ref")
                .attr("loc", bind.getValue())
                .up()
                .up();
        }
        return dirs;
    }
}
