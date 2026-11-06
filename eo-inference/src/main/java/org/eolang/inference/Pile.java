/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;

/**
 * Numbers in a pile, the last one on top.
 *
 * <p>The walk keeps three piles: the questions it is asking right now, one
 * inside the other, the copies it still has to walk in this pass, and the
 * answers that lost theirs and wait to be worked out again. A number may
 * be in a pile twice, since none of them asks whether it holds one.</p>
 *
 * @since 0.76.0
 */
final class Pile {

    /**
     * The numbers, the last one on top.
     */
    private int[] items;

    /**
     * How many of {@link #items} are taken.
     */
    private int size;

    /**
     * Ctor.
     */
    Pile() {
        this(new int[16], 0);
    }

    /**
     * Ctor.
     *
     * @param array The numbers
     * @param taken How many of them are taken
     */
    private Pile(final int[] array, final int taken) {
        this.items = array;
        this.size = taken;
    }

    /**
     * Put a number on top.
     *
     * @param number The number
     */
    void push(final int number) {
        if (this.size == this.items.length) {
            this.items = Arrays.copyOf(this.items, this.size * 2);
        }
        this.items[this.size] = number;
        this.size += 1;
    }

    /**
     * Take the number on top off.
     *
     * @return The number
     */
    int pop() {
        this.size -= 1;
        return this.items[this.size];
    }

    /**
     * The number on top, which stays there.
     *
     * @return The number
     */
    int top() {
        return this.items[this.size - 1];
    }

    /**
     * The number at a position, from the bottom.
     *
     * @param idx The position
     * @return The number
     */
    int get(final int idx) {
        return this.items[idx];
    }

    /**
     * How many numbers are here.
     *
     * @return The count
     */
    int size() {
        return this.size;
    }

    /**
     * Whether nothing is here.
     *
     * @return TRUE if nothing is
     */
    boolean isEmpty() {
        return this.size == 0;
    }

    /**
     * Take every number off.
     */
    void clear() {
        this.size = 0;
    }
}
