/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.AbstractMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * A {@link Map} of attributes built from a varargs collection of
 * {@link Map.Entry} instances.
 *
 * <p>This class exists so that subclasses of {@link PhDefault} can pass
 * their initial attributes through {@code super(...)} without making any
 * method calls inside their own constructor body. The trick relies on
 * {@link AbstractMap.SimpleEntry} (a constructor invocation, not a method
 * call) being acceptable as a {@code Map.Entry} factory.</p>
 *
 * <p>The underlying {@link LinkedHashMap} is built lazily on first access, so
 * the constructor does nothing but copy the array it was given. The copy of
 * the array, together with the immutability of {@link Attr}, is what keeps the
 * map immune to a caller that changes what it handed over.</p>
 *
 * @since 0.59
 */
public final class Attrs extends AbstractMap<String, Attribute> {

    /**
     * Initial entries supplied via constructor, our own copy of them.
     */
    private final Attr[] entries;

    /**
     * Lazily-resolved backing map.
     */
    private Map<String, Attribute> resolved;

    /**
     * Ctor.
     *
     * @param initial Entries to populate the map with
     */
    public Attrs(final Attr... initial) {
        super();
        this.entries = initial.clone();
    }

    @Override
    public Set<Map.Entry<String, Attribute>> entrySet() {
        return this.resolve().entrySet();
    }

    @Override
    public int size() {
        return this.resolve().size();
    }

    private Map<String, Attribute> resolve() {
        if (this.resolved == null) {
            final Map<String, Attribute> map = new LinkedHashMap<>(this.entries.length);
            for (final Map.Entry<String, Attribute> ent : this.entries) {
                map.put(ent.getKey(), ent.getValue());
            }
            this.resolved = map;
        }
        return this.resolved;
    }
}
