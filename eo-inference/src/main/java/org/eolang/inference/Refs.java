/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The pairs and the binds, as the rows of a table.
 *
 * <p>Two things are worked out apart and belong together in one row: what an
 * object is a copy of, and which voids of that copy it has filled. Neither is
 * of any use without the other — a copy nobody has given arguments to and a
 * saturated one are the same pair and different types.</p>
 *
 * @since 0.69.0
 */
final class Refs {

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
     *
     * @param pairs The pairs, each object against the one it is a copy of
     * @param binds What every application put into the voids of what it
     *  copies, from {@link Bound}
     */
    Refs(final Map<String, String> pairs, final Map<String, Map<String, String>> binds) {
        this.copies = pairs;
        this.filled = binds;
    }

    /**
     * These pairs as rows.
     *
     * @return The types, by the locator of the object they are about, in the
     *  order the pairs came in
     */
    Map<String, Type> all() {
        final Map<String, Type> found = new LinkedHashMap<>(this.copies.size());
        for (final Map.Entry<String, String> pair : this.copies.entrySet()) {
            found.put(
                pair.getKey(),
                new Ref(
                    pair.getValue(),
                    this.filled.getOrDefault(pair.getKey(), Collections.emptyMap())
                )
            );
        }
        return found;
    }
}
