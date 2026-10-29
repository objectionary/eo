/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;

/**
 * A set of numbers that grows, in the order they came.
 *
 * <p>The walk keeps almost everything as numbers: a value is the number it
 * was given when it was first made, and so is every question the memo
 * holds. A row of a void, the answer a read is working out, the readers of
 * an answer: each is a set of such numbers that only grows while the walk
 * goes, and is asked again and again whether it holds one. A Java set of
 * boxed numbers costs ten times the memory, which eo-runtime cannot
 * afford, so this one keeps the numbers in an array, with a table beside
 * it once it holds more than a few.</p>
 *
 * @since 0.76.0
 */
final class Ints {

    /**
     * How many numbers are looked for one by one, before a table is kept.
     */
    private static final int FEW = 8;

    /**
     * The numbers, in the order they came.
     */
    private int[] items;

    /**
     * How many of {@link #items} are taken.
     */
    private int size;

    /**
     * The table of positions, each one the number plus one, or empty.
     */
    private int[] slots;

    /**
     * Ctor.
     */
    Ints() {
        this(new int[2], 0, new int[0]);
    }

    /**
     * Ctor.
     *
     * @param numbers The numbers to start with
     */
    Ints(final int... numbers) {
        this(Ints.filled(numbers));
    }

    /**
     * Ctor.
     *
     * @param other The set whose numbers and table it takes over
     */
    private Ints(final Ints other) {
        this(other.items, other.size, other.slots);
    }

    /**
     * Ctor.
     *
     * @param array The numbers
     * @param taken How many of them are taken
     * @param table The table of positions
     */
    private Ints(final int[] array, final int taken, final int... table) {
        this.items = array;
        this.size = taken;
        this.slots = table;
    }

    /**
     * Add a number.
     *
     * @param number The number
     * @return TRUE if it was not there before
     */
    boolean add(final int number) {
        final boolean fresh = !this.contains(number);
        if (fresh) {
            if (this.size == this.items.length) {
                this.items = Arrays.copyOf(this.items, this.size * 2);
            }
            this.items[this.size] = number;
            this.size += 1;
            if (this.slots.length > 0) {
                this.place(number);
            }
            if (this.size > Ints.FEW && this.slots.length < this.size * 2) {
                this.rebuild();
            }
        }
        return fresh;
    }

    /**
     * Add every number of an answer.
     *
     * @param ids The answer
     */
    void addAll(final Ids ids) {
        for (int idx = 0; idx < ids.size(); ++idx) {
            this.add(ids.get(idx));
        }
    }

    /**
     * Add every number of another set.
     *
     * @param other The other set
     */
    void addAll(final Ints other) {
        for (int idx = 0; idx < other.size(); ++idx) {
            this.add(other.get(idx));
        }
    }

    /**
     * Take a number out.
     *
     * @param number The number
     */
    void remove(final int number) {
        if (this.contains(number)) {
            int pos = 0;
            while (this.items[pos] != number) {
                pos += 1;
            }
            System.arraycopy(this.items, pos + 1, this.items, pos, this.size - pos - 1);
            this.size -= 1;
            if (this.slots.length > 0) {
                this.rebuild();
            }
        }
    }

    /**
     * Whether a number is here.
     *
     * @param number The number
     * @return TRUE if it is
     */
    boolean contains(final int number) {
        boolean found = false;
        if (this.slots.length == 0) {
            for (int idx = 0; idx < this.size; ++idx) {
                if (this.items[idx] == number) {
                    found = true;
                    break;
                }
            }
        } else {
            final int mask = this.slots.length - 1;
            int slot = Ints.hash(number) & mask;
            while (this.slots[slot] != 0) {
                if (this.slots[slot] == number + 1) {
                    found = true;
                    break;
                }
                slot = slot + 1 & mask;
            }
        }
        return found;
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
     * The number at a position, in the order they came.
     *
     * @param idx The position
     * @return The number
     */
    int get(final int idx) {
        return this.items[idx];
    }

    /**
     * The numbers, in the order they came, as an array of their own.
     *
     * @return The array
     */
    int[] array() {
        return Arrays.copyOf(this.items, this.size);
    }

    /**
     * The numbers as an answer, which never changes.
     *
     * @return The answer
     */
    Ids frozen() {
        final int[] sorted = this.array();
        Arrays.sort(sorted);
        return new Ids(sorted);
    }

    /**
     * Forget every number.
     */
    void clear() {
        this.size = 0;
        this.items = new int[2];
        this.slots = new int[0];
    }

    private void rebuild() {
        int length = Ints.FEW * 2;
        while (length < this.size * 4) {
            length *= 2;
        }
        this.slots = new int[length];
        for (int idx = 0; idx < this.size; ++idx) {
            this.place(this.items[idx]);
        }
    }

    private void place(final int number) {
        final int mask = this.slots.length - 1;
        int slot = Ints.hash(number) & mask;
        while (this.slots[slot] != 0) {
            slot = slot + 1 & mask;
        }
        this.slots[slot] = number + 1;
    }

    private static Ints filled(final int... numbers) {
        final Ints ints = new Ints();
        for (final int number : numbers) {
            ints.add(number);
        }
        return ints;
    }

    private static int hash(final int number) {
        final int mixed = number * 0x9E3779B9;
        return mixed ^ mixed >>> 16;
    }
}
