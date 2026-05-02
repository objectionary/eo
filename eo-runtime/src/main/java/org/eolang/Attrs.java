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
 * <p>The underlying {@link HashMap} is built lazily on first access so the
 * constructor itself remains free of method calls.</p>
 *
 * @since 0.59
 */
public final class Attrs extends AbstractMap<String, Attr> {

    /**
     * Initial entries supplied via constructor.
     * @checkstyle VisibilityModifierCheck (2 lines)
     */
    private final Map.Entry<String, Attr>[] entries;

    /**
     * Lazily-resolved backing map.
     */
    private Map<String, Attr> resolved;

    /**
     * Ctor.
     * @param initial Entries to populate the map with
     */
    @SafeVarargs
    @SuppressWarnings("PMD.ArrayIsStoredDirectly")
    public Attrs(final Map.Entry<String, Attr>... initial) {
        super();
        this.entries = initial;
    }

    @Override
    public Set<Map.Entry<String, Attr>> entrySet() {
        return this.resolve().entrySet();
    }

    @Override
    public int size() {
        return this.resolve().size();
    }

    /**
     * Resolve the entries into a backing map, lazily.
     * @return The backing map
     */
    private Map<String, Attr> resolve() {
        if (this.resolved == null) {
            final Map<String, Attr> map = new LinkedHashMap<>(this.entries.length);
            for (final Map.Entry<String, Attr> ent : this.entries) {
                map.put(ent.getKey(), ent.getValue());
            }
            this.resolved = map;
        }
        return this.resolved;
    }
}
