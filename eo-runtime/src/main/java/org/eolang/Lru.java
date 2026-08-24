/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * A map that holds no more than the given number of entries, letting
 * the entry asked for longest ago go first.
 *
 * <p>The map is not thread-safe on its own — wrap it in
 * {@link java.util.Collections#synchronizedMap(Map)} when several
 * threads share it.</p>
 *
 * <p>A capacity below one keeps nothing: every {@code put} is accepted
 * and forgotten, so a caller that asks for no cache gets no cache rather
 * than a failure.</p>
 *
 * @since 0.75
 */
final class Lru implements Map<String, byte[]> {

    /**
     * The entries, in the order of access.
     */
    private final Map<String, byte[]> origin;

    /**
     * How many entries to keep.
     */
    private final int capacity;

    /**
     * Ctor.
     * @param cap How many entries to keep
     */
    Lru(final int cap) {
        this.origin = new LinkedHashMap<>(16, 0.75f, true);
        this.capacity = cap;
    }

    @Override
    public int size() {
        return this.origin.size();
    }

    @Override
    public boolean isEmpty() {
        return this.origin.isEmpty();
    }

    @Override
    public boolean containsKey(final Object key) {
        return this.origin.containsKey(key);
    }

    @Override
    public boolean containsValue(final Object value) {
        return this.origin.containsValue(value);
    }

    @Override
    public byte[] get(final Object key) {
        return this.origin.get(key);
    }

    @Override
    public byte[] put(final String key, final byte[] value) {
        final byte[] before;
        if (this.capacity < 1) {
            before = this.origin.remove(key);
        } else {
            if (this.origin.size() >= this.capacity && !this.origin.containsKey(key)) {
                final Iterator<String> eldest = this.origin.keySet().iterator();
                eldest.next();
                eldest.remove();
            }
            before = this.origin.put(key, value);
        }
        return before;
    }

    @Override
    public byte[] remove(final Object key) {
        return this.origin.remove(key);
    }

    @Override
    public void putAll(final Map<? extends String, ? extends byte[]> map) {
        for (final Map.Entry<? extends String, ? extends byte[]> entry : map.entrySet()) {
            this.put(entry.getKey(), entry.getValue());
        }
    }

    @Override
    public void clear() {
        this.origin.clear();
    }

    @Override
    public Set<String> keySet() {
        return this.origin.keySet();
    }

    @Override
    public Collection<byte[]> values() {
        return this.origin.values();
    }

    @Override
    public Set<Map.Entry<String, byte[]>> entrySet() {
        return this.origin.entrySet();
    }
}
