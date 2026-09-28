/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The name every type of the program behaves as, as the table says it.
 *
 * <p>{@link Behaved} works the name out and {@link Reduced} writes it on the
 * row, so whoever comes after them has only to read the cell. It is read back
 * rather than worked out again because the two are the same fact, and a
 * program whose pages say one thing and whose census counts another has no
 * answer to give about which of them is the program.</p>
 *
 * <p>A row that behaves as itself says nothing and is left out, so what comes
 * back is the types that go by another name, and a reader who finds nothing
 * here about a type has the name it came with.</p>
 *
 * @since 0.73.0
 */
final class Behaviours {

    /**
     * The provides table.
     */
    private final XML table;

    /**
     * Ctor.
     *
     * @param provides The provides table, as {@link Reduced} left it
     */
    Behaviours(final XML provides) {
        this.table = provides;
    }

    /**
     * The name every type behaves as.
     *
     * @return The name to go by, by the locator of the type, without the types
     *  that behave as themselves
     */
    Map<String, String> all() {
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final Xnav row : new Rows(this.table).all()) {
            final Noted noted = new Noted(row);
            final String behaves = noted.says("reduced");
            if (!behaves.isEmpty()) {
                found.put(noted.says("id"), behaves);
            }
        }
        return found;
    }
}
