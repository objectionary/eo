/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * What every void says it will hold.
 *
 * <p>A void holds whatever a caller puts in it, and the source is sometimes
 * able to say what that will be: a formation only Java ever copies has no
 * caller to read the answer off, so it writes the answer down instead, as the
 * annotation of {@code ? > code /Q.number}. {@link Provides} keeps it, and
 * this reads it back by the locator of the void.</p>
 *
 * <p>An annotation may end in a question mark, which says the value is that
 * type or a termination. It is dropped here, since the two answer to the same
 * names: a termination answers to every name there is.</p>
 *
 * @since 0.69.0
 */
final class Held {

    /**
     * The provides table.
     */
    private final XML table;

    /**
     * Ctor.
     * @param provides The provides table, as {@link Provides} wrote it
     */
    Held(final XML provides) {
        this.table = provides;
    }

    /**
     * What every void that says so will hold.
     * @return The types, by the locator of the void, without the voids that
     *  say nothing
     */
    Map<String, String> all() {
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final XML attr : this.table.nodes("//attr[@void='true' and @holds]")) {
            final Noted row = new Noted(attr);
            found.put(row.says("type"), row.says("holds").replace("?", ""));
        }
        return found;
    }
}
