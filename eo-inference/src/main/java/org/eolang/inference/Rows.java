/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Slots and stamps (design doc 3.2): what each bound void of each copy
 * holds, and values put onto a holder.
 *
 * <p>What a bound void holds is a set, kept in a table with one row per
 * copy and void, which only grows; {@code ρ} is a row like any other. A
 * blank copy put into a {@code ρ} joins the shared one. A row that grows
 * is a change, and a new value in a {@code ρ} row changes which copies may
 * count.</p>
 *
 * <p>A <b>stamp</b> puts a value read out of a row onto its holder: a copy
 * with no {@code ρ} becomes a new copy at the place of the read, with the
 * holder as its {@code ρ} and the rows of the copy it came from, which it
 * takes again on every pass.</p>
 *
 * @since 0.76.0
 */
final class Rows {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The rows, by copy and void.
     */
    private final Map<Slot, Ints> table;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Rows(final State walk) {
        this.state = walk;
        this.table = new HashMap<>(0);
    }

    /**
     * Put a value into a row before the walk starts.
     *
     * @param copy The copy
     * @param name The void
     * @param value The value
     */
    void seed(final int copy, final String name, final int value) {
        this.table.computeIfAbsent(new Slot(copy, name), key -> new Ints()).add(value);
    }

    /**
     * Gather values into a row, which grows.
     *
     * @param copy The copy
     * @param name The void
     * @param values The values
     */
    void gather(final int copy, final String name, final int[] values) {
        final Ints row = this.table.computeIfAbsent(new Slot(copy, name), key -> new Ints());
        final int size = row.size();
        final boolean rho = "ρ".equals(name);
        for (final int value : values) {
            int put = value;
            if (rho) {
                put = this.state.copies.joined(value);
            }
            if (row.add(put)) {
                this.state.passes.change();
                if (rho) {
                    this.state.counting.grow();
                }
            }
        }
        if (row.size() > size) {
            this.state.memo.spoil(new Key('r', copy, name, "", "", 0));
        }
    }

    /**
     * What a row holds now, as read by whoever is being asked.
     *
     * @param copy The copy
     * @param name The void
     * @return The values
     */
    int[] cell(final int copy, final String name) {
        this.state.memo.depend(new Key('r', copy, name, "", "", 0));
        return this.row(copy, name).array();
    }

    /**
     * A row, read by nobody.
     *
     * @param copy The copy
     * @param name The void
     * @return The row, empty when there is none
     */
    Ints row(final int copy, final String name) {
        return this.table.getOrDefault(new Slot(copy, name), new Ints());
    }

    /**
     * Whether a row is there, even an empty one.
     *
     * @param copy The copy
     * @param name The void
     * @return TRUE if it is
     */
    boolean has(final int copy, final String name) {
        return this.table.containsKey(new Slot(copy, name));
    }

    /**
     * Every row, by copy and void.
     *
     * @return The rows
     */
    Map<Slot, Ints> all() {
        return this.table;
    }

    /**
     * Put a value read out of a row onto its holder.
     *
     * @param num The value
     * @param holder The holder
     * @param place The place of the read
     * @param tag The tag of the reader, empty when none
     * @return The value on its holder
     */
    int stamp(final int num, final int holder, final String place, final String tag) {
        final Value value = this.state.values.get(num);
        final int result;
        if (value.kind() == 'B') {
            result = this.state.reduction.await(
                this.stamp(value.base(), holder, place, tag)
            );
        } else if (value.kind() == 'A') {
            result = this.state.reduction.mark(
                this.stamp(value.base(), holder, place, tag), value.count()
            );
        } else if (value.kind() == 'O' && (value.bound() & Formation.RHO) == 0) {
            final Map<String, int[]> rows = new LinkedHashMap<>(0);
            for (final String name : this.state.values.bound(value)) {
                rows.put(name, this.cell(num, name));
            }
            rows.put("ρ", new int[] {holder});
            result = this.state.copies.copy(
                value.form(), place, value.bound() | Formation.RHO, rows,
                this.state.tags.tagged(tag, value.form(), place)
            );
        } else {
            result = num;
        }
        return result;
    }
}
