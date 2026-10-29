/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;

/**
 * An answer: the values a read or a reduction gives, which never change.
 *
 * <p>An answer only grows from pass to pass, and it grows by being replaced
 * with a bigger one, never by being changed in place, so that whoever holds
 * the old one still holds what it was told. Equal answers are one object
 * in the memo (design doc 6.5, {@code 89859f4}): only a few tens of
 * thousands of the million answers differ, and most hold one value.</p>
 *
 * @since 0.76.0
 */
final class Ids {

    /**
     * The values, in ascending order.
     */
    private final int[] values;

    /**
     * Ctor.
     *
     * @param sorted The values, in ascending order, each once
     */
    Ids(final int... sorted) {
        this.values = Arrays.copyOf(sorted, sorted.length);
    }

    @Override
    public boolean equals(final Object other) {
        return this == other
            || other instanceof Ids ids && Arrays.equals(this.values, ids.values);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(this.values);
    }

    @Override
    public String toString() {
        return Arrays.toString(this.values);
    }

    /**
     * How many values there are.
     *
     * @return The count
     */
    int size() {
        return this.values.length;
    }

    /**
     * Whether there is none.
     *
     * @return TRUE if there is none
     */
    boolean isEmpty() {
        return this.values.length == 0;
    }

    /**
     * The value at a position.
     *
     * @param idx The position
     * @return The value
     */
    int get(final int idx) {
        return this.values[idx];
    }

    /**
     * Whether a value is here.
     *
     * @param value The value
     * @return TRUE if it is
     */
    boolean contains(final int value) {
        return Arrays.binarySearch(this.values, value) >= 0;
    }

    /**
     * Whether every value of another set is here.
     *
     * @param other The other set
     * @return TRUE if every one is
     */
    boolean holds(final Ints other) {
        boolean all = true;
        for (int idx = 0; idx < other.size(); ++idx) {
            if (!this.contains(other.get(idx))) {
                all = false;
                break;
            }
        }
        return all;
    }

    /**
     * This answer with the values of another set added.
     *
     * @param other The other set
     * @return The bigger answer
     */
    Ids with(final Ints other) {
        final Ints all = new Ints(this.values);
        all.addAll(other);
        return all.frozen();
    }
}
