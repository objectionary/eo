/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * What the links table says, pair by pair.
 *
 * <p>{@link Links} writes one row per name that is a copy of another, and
 * reading them back is how a later pass adds to them without losing what is
 * there. The order they were written in is kept, so a document read and written
 * again keeps the rules' rows where they were and carries the worked-out ones
 * after them.</p>
 *
 * @since 0.68.0
 */
final class Pairs {

    /**
     * The links table.
     */
    private final XML table;

    /**
     * Ctor.
     * @param links The links table
     */
    Pairs(final XML links) {
        this.table = links;
    }

    /**
     * Every pair of the table.
     * @return The pairs, each name against the one it is a copy of
     */
    Map<String, String> all() {
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final XML link : this.table.nodes("/links/type")) {
            found.put(link.xpath("@id").get(0), link.xpath("@copy").get(0));
        }
        return found;
    }
}
