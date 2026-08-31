/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;

/**
 * Attributes of one object, kept in a pair of lists.
 *
 * <p>An object carries a handful of attributes and reads them by name, which
 * a {@link java.util.HashMap} serves at the price of a table, a node per
 * entry, and a hash of every name on every read. Two lists and a walk over
 * them cost a fraction of that and are quicker at this size, because the
 * names are string literals from the generated code and almost always match
 * by reference alone.</p>
 *
 * @since 0.63
 */
final class Bindings extends AbstractMap<String, Attribute> implements Walkable {

    /**
     * Names of the attributes, in the order they arrived.
     */
    private final List<String> names;

    /**
     * Attributes themselves, at the same positions as their names.
     */
    private final List<Attribute> values;

    /**
     * Ctor.
     */
    Bindings() {
        super();
        this.names = new ArrayList<>(0);
        this.values = new ArrayList<>(0);
    }

    @Override
    public int size() {
        return this.names.size();
    }

    @Override
    public boolean containsKey(final Object key) {
        return this.names.indexOf(key) >= 0;
    }

    @Override
    public Attribute get(final Object key) {
        final int idx = this.names.indexOf(key);
        final Attribute found;
        if (idx < 0) {
            found = null;
        } else {
            found = this.values.get(idx);
        }
        return found;
    }

    @Override
    public Attribute put(final String key, final Attribute value) {
        final int idx = this.names.indexOf(key);
        final Attribute before;
        if (idx < 0) {
            this.names.add(key);
            this.values.add(value);
            before = null;
        } else {
            before = this.values.set(idx, value);
        }
        return before;
    }

    @Override
    public Set<Map.Entry<String, Attribute>> entrySet() {
        final Map<String, Attribute> all = new LinkedHashMap<>(this.names.size());
        for (int idx = 0; idx < this.names.size(); ++idx) {
            all.put(this.names.get(idx), this.values.get(idx));
        }
        return all.entrySet();
    }

    @Override
    public void each(final BiConsumer<String, Attribute> action) {
        for (int idx = 0; idx < this.names.size(); ++idx) {
            action.accept(this.names.get(idx), this.values.get(idx));
        }
    }
}
