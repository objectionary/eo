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
 * the constructor does nothing but copy the key and the value of every entry
 * it was given. The copy is what keeps the map immune to a caller that
 * mutates its own array, or any entry inside it, after handing it over.</p>
 *
 * @since 0.59
 */
public final class Attrs extends AbstractMap<String, Attribute> {

    /**
     * Names of the initial entries, our own copy of them.
     */
    private final String[] names;

    /**
     * Attributes of the initial entries, our own copy of them.
     */
    private final Attribute[] attributes;

    /**
     * Lazily-resolved backing map.
     */
    private Map<String, Attribute> resolved;

    /**
     * Ctor.
     * @param initial Entries to populate the map with
     */
    @SafeVarargs
    public Attrs(final Map.Entry<String, Attribute>... initial) {
        super();
        this.names = new String[initial.length];
        this.attributes = new Attribute[initial.length];
        for (int idx = 0; idx < initial.length; ++idx) {
            this.names[idx] = initial[idx].getKey();
            this.attributes[idx] = initial[idx].getValue();
        }
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
            final Map<String, Attribute> map = new LinkedHashMap<>(this.names.length);
            for (int idx = 0; idx < this.names.length; ++idx) {
                map.put(this.names[idx], this.attributes[idx]);
            }
            this.resolved = map;
        }
        return this.resolved;
    }
}
