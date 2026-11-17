/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;

/**
 * A number noted for each value, zero for a value nothing was noted of.
 *
 * <p>The walk asks of every value, again and again, whether it has an empty
 * void of its own and whether it counts, and keeps what it worked out by
 * the number of the value.</p>
 *
 * @since 0.76.0
 */
final class Marks {

    /**
     * The numbers noted, by value.
     */
    private int[] items;

    /**
     * Ctor.
     */
    Marks() {
        this(new int[1024]);
    }

    /**
     * Ctor.
     *
     * @param array The numbers noted, by value
     */
    private Marks(final int... array) {
        this.items = array;
    }

    /**
     * The number noted of a value.
     *
     * @param num The value
     * @return The number, zero when nothing was noted
     */
    int get(final int num) {
        final int got;
        if (num < this.items.length) {
            got = this.items[num];
        } else {
            got = 0;
        }
        return got;
    }

    /**
     * Note a number of a value.
     *
     * @param num The value
     * @param mark The number
     */
    void put(final int num, final int mark) {
        if (num >= this.items.length) {
            this.items = Arrays.copyOf(this.items, Math.max(num + 1, this.items.length * 2));
        }
        this.items[num] = mark;
    }
}
