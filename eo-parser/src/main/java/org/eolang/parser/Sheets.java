/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * The bookkeeping list of sheets a document has been through. Every
 * shift appends its own name here, to a copy of the document -
 * nothing else about the tree is read or rebuilt.
 *
 * @since 0.62.2
 */
final class Sheets {

    /**
     * The name of the sheet to record.
     */
    private final String name;

    /**
     * Ctor.
     *
     * @param sheet The name to record
     */
    Sheets(final String sheet) {
        this.name = sheet;
    }

    /**
     * The document with the sheet recorded in it - one whose root is
     * not an object keeps no record and comes back as it is.
     *
     * @param xml The document
     * @return A copy with one more {@code sheet} in its {@code sheets}
     */
    XML added(final XML xml) {
        return new XMLDocument(
            new Xembler(
                new Directives()
                    .xpath("/object")
                    .addIf("sheets")
                    .add("sheet")
                    .set(this.name)
            ).applyQuietly(new Xnav(xml.inner()).copy().node())
        );
    }
}
