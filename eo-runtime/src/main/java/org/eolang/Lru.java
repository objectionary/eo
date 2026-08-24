/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * A map that holds no more than the given number of entries, letting
 * the entry asked for longest ago go first.
 *
 * <p>{@link LinkedHashMap} in access order is the one map the JDK lets
 * evict by age of use, and the extension point it offers for that is an
 * override, not an argument. The map is not thread-safe on its own —
 * wrap it in {@link java.util.Collections#synchronizedMap(Map)} when
 * several threads share it.</p>
 *
 * @since 0.75
 */
final class Lru extends LinkedHashMap<String, byte[]> {

    /**
     * Serialization marker, demanded by the parent.
     */
    private static final long serialVersionUID = 5165L;

    /**
     * How many entries to keep.
     */
    private final int capacity;

    /**
     * Ctor.
     * @param cap How many entries to keep
     */
    Lru(final int cap) {
        super(16, 0.75f, true);
        this.capacity = cap;
    }

    @Override
    protected boolean removeEldestEntry(final Map.Entry<String, byte[]> eldest) {
        return this.size() > this.capacity;
    }
}
