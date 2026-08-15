/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import org.xembly.Directives;

/**
 * A type read from the table and written back as it was.
 *
 * <p>A pass that works out more than the table says has to write the table
 * again, and it can only build the kinds of answer it knows about. Anything
 * else would fall out of the document, silently and completely, the first
 * time a later pass ran — a row saying an object is a datum vanishing
 * because the pass that resolves dispatches has no business with data. So
 * whatever is not rebuilt is kept exactly as it was found.</p>
 *
 * @since 0.69.0
 */
final class Kept implements Type {

    /**
     * The row, as the document keeps it.
     */
    private final XML row;

    /**
     * Ctor.
     * @param written The row, as the document keeps it, rooted at the row
     *  itself and not at a document of its own
     */
    Kept(final XML written) {
        this.row = written;
    }

    @Override
    public Directives directives() {
        final Directives dirs = new Directives();
        for (final XML held : this.row.nodes("*")) {
            dirs.add(held.inner().getNodeName())
                .append(Directives.copyOf(held.inner()))
                .up();
        }
        return dirs;
    }
}
