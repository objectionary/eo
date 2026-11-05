/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Copies made for calls back (design doc 3.5, rule 1).
 *
 * <p>A void written {@code ? > name /{T1 … Tn}} is one its atom calls back.
 * Where a call fills such a void, each value handed for it that has an empty
 * void gets one more copy at the call, its first n empty voids filled with
 * Java's own T1 … Tn, and that copy is walked: {@code malloc.of 5 [m]}
 * gives {@code m} a {@code Φ.chunk}, as Java does. Each of T1 … Tn is a
 * type tree ({@link Annotation}), so its void gets one copy Java makes for
 * each option of a union, with the voids of that copy typed by its
 * brackets.</p>
 *
 * @since 0.76.0
 */
final class Callbacks {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The world the walk goes over.
     */
    private final World world;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     * @param world The world the walk goes over
     */
    Callbacks(final State walk, final World world) {
        this.state = walk;
        this.world = world;
    }

    /**
     * Make the copy an atom calls a value back with.
     *
     * @param num The value handed
     * @param types The types the atom calls it back with
     * @param site The site of the call, under the tag of the caller
     */
    void called(final int num, final List<String> types, final Site site) {
        final Value value = this.world.value(num);
        if (value.kind() == 'O' && !this.world.opened(value).isEmpty()) {
            final List<String> holes = this.world.opened(value);
            final Map<String, int[]> rows = new LinkedHashMap<>(0);
            for (final String name : this.world.bound(value)) {
                rows.put(name, this.state.cell(num, name));
            }
            final int count = Math.min(holes.size(), types.size());
            for (int idx = 0; idx < count; ++idx) {
                final Ints made = new Ints();
                for (final Annotation option : new Annotation(types.get(idx)).options()) {
                    final int copy = this.state.java(option);
                    this.state.remember(copy);
                    made.add(copy);
                }
                rows.put(holes.get(idx), made.array());
            }
            this.state.remember(
                this.state.copy(
                    new Value(
                        value.form(), this.world.born(site, value.form()),
                        value.bound() | this.state.bits(value.form(), holes.subList(0, count))
                    ),
                    rows
                )
            );
        }
    }
}
