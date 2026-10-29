/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.BitSet;
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
 * gives {@code m} a {@code Φ.chunk}, as Java does.</p>
 *
 * <p>A copy handed to an atom whose {@code ^} is a declared void with no
 * type and is not bound gets it from Java alone, and the answers say so.</p>
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
     * The copies handed to an atom whose {@code ρ} only Java fills.
     */
    private final BitSet handed;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     * @param world The world the walk goes over
     */
    Callbacks(final State walk, final World world) {
        this.state = walk;
        this.world = world;
        this.handed = new BitSet();
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
                final int made = this.state.java(types.get(idx));
                this.state.remember(made);
                rows.put(holes.get(idx), new int[] {made});
            }
            final int copy = this.state.copy(
                new Value(
                    value.form(), this.world.born(site, value.form()),
                    value.bound() | this.state.bits(value.form(), holes.subList(0, count))
                ),
                rows
            );
            this.state.remember(copy);
            if (this.handed.get(num)) {
                this.handed.set(copy);
            }
        }
    }

    /**
     * Note a value handed to an atom: a copy whose {@code ρ} is a declared
     * void with no type, and not bound, gets it from Java alone.
     *
     * @param num The value
     */
    void handed(final int num) {
        final int inside = this.world.bare(num);
        final Value value = this.world.value(inside);
        if (value.kind() == 'O' && value.placed() && (value.bound() & Formation.RHO) == 0) {
            final Formation shape = this.world.form(value.form());
            if (shape.open("ρ") && shape.rho().isEmpty()) {
                this.handed.set(inside);
            }
        }
    }

    /**
     * Whether only Java fills the {@code ρ} of a copy handed to an atom.
     *
     * @param num The copy
     * @return TRUE if only Java does
     */
    boolean handedToJava(final int num) {
        return this.handed.get(num);
    }
}
