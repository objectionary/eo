/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.HashMap;
import java.util.Map;

/**
 * The name a type and all its copies go by.
 *
 * <p>{@link Links} says that one type is a copy of another, and for now a
 * copy is the same thing as what it copies: whatever one of them has or is
 * asked for, the other one has or is asked for too. A chain of copies is
 * therefore one type under several names, and the tables are easiest to read
 * when the whole chain is called by one of them — the one at the end of the
 * chain, which is the formation or the void the copies all come from, and
 * which is a name a human recognises.</p>
 *
 * <p>This is the one place where the checker later gets smarter. When a copy
 * begins to receive types of its own, chains stop collapsing and nothing else
 * has to change, because every question about a type goes through the name
 * handed out here first.</p>
 *
 * @since 0.68.0
 */
final class Same {

    /**
     * The links table.
     */
    private final XML table;

    /**
     * Ctor.
     * @param links The links table
     */
    Same(final XML links) {
        this.table = links;
    }

    /**
     * The name every type of the table goes by.
     * @return The name, by type; a type the table says nothing about is
     *  absent from the map, since it goes by its own name
     */
    Map<String, String> names() {
        final Map<String, String> copies = new HashMap<>(0);
        for (final XML link : this.table.nodes("/links/type")) {
            copies.put(link.xpath("@id").get(0), link.xpath("@copy").get(0));
        }
        return new Ends(copies).names();
    }
}
