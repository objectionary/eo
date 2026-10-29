/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Every value the walk has made, each under the number it got first.
 *
 * <p>A value is made of what it is (3.1): two copies of the same formation,
 * born at the same place, with the same tag and the same voids bound, are
 * one copy, and are one number. The first six numbers are the root, a datum,
 * a termination, Java's mark, an open void, and an open void a stop
 * decided.</p>
 *
 * @since 0.76.0
 */
final class Values {

    /**
     * The root, {@code Φ}.
     */
    static final int ROOT = 0;

    /**
     * A datum.
     */
    static final int DATA = 1;

    /**
     * A termination, {@code T}.
     */
    static final int TERM = 2;

    /**
     * What only Java puts into a {@code ρ}.
     */
    static final int JAVA = 3;

    /**
     * An open void.
     */
    static final int GAP = 4;

    /**
     * An open void a stop decided Java alone fills (4.4).
     */
    static final int JGAP = 5;

    /**
     * The birthplace of every copy Java makes.
     */
    static final String JBORN = "java";

    /**
     * The program, which names the voids of a copy.
     */
    private final Program program;

    /**
     * The values, by number.
     */
    private final List<Value> all;

    /**
     * The numbers, by value.
     */
    private final Map<Value, Integer> index;

    /**
     * Ctor.
     *
     * @param prog The program
     */
    Values(final Program prog) {
        this(prog, new ArrayList<>(0), new HashMap<>(0));
    }

    /**
     * Ctor.
     *
     * @param prog The program
     * @param list The values, by number
     * @param numbers The numbers, by value
     */
    private Values(final Program prog, final List<Value> list, final Map<Value, Integer> numbers) {
        this.program = prog;
        this.all = list;
        this.index = numbers;
    }

    /**
     * The number of a value, given now if it has none.
     *
     * @param value The value
     * @return The number
     */
    int intern(final Value value) {
        Integer num = this.index.get(value);
        if (num == null) {
            if (this.all.isEmpty()) {
                this.start();
            }
            num = this.index.get(value);
            if (num == null) {
                num = this.all.size();
                this.all.add(value);
                this.index.put(value, num);
            }
        }
        return num;
    }

    /**
     * The value of a number.
     *
     * @param num The number
     * @return The value
     */
    Value get(final int num) {
        return this.all.get(num);
    }

    /**
     * How many values there are.
     *
     * @return The count
     */
    int size() {
        return this.all.size();
    }

    /**
     * An unknown, for a reason.
     *
     * @param why The reason
     * @return Its number
     */
    int unknown(final String why) {
        return this.intern(new Value('U', why, "", 0L, "", -1, 0));
    }

    /**
     * A failing read.
     *
     * @param label What fails
     * @return Its number
     */
    int conflict(final String label) {
        return this.intern(new Value('X', label, "", 0L, "", -1, 0));
    }

    /**
     * Whether a value is an open void, decided or not.
     *
     * @param num The number
     * @return TRUE if it is
     */
    boolean gap(final int num) {
        final Value value = this.get(num);
        return value.kind() == 'U' && "open void".equals(value.form());
    }

    /**
     * The value with the rule b and rule A wrappers taken off.
     *
     * @param num The number
     * @return The number of what is inside
     */
    int bare(final int num) {
        int inside = num;
        while (this.get(inside).kind() == 'A' || this.get(inside).kind() == 'B') {
            inside = this.get(inside).base();
        }
        return inside;
    }

    /**
     * The voids of a copy that are bound, in the order of its formation,
     * {@code ρ} last.
     *
     * @param value The copy
     * @return The voids
     */
    List<String> bound(final Value value) {
        final List<String> names = new ArrayList<>(0);
        final List<String> order = this.program.form(value.form()).order();
        for (int pos = 0; pos < order.size(); ++pos) {
            if ((value.bound() & 1L << pos) != 0) {
                names.add(order.get(pos));
            }
        }
        if ((value.bound() & Formation.RHO) != 0) {
            names.add("ρ");
        }
        return names;
    }

    /**
     * The voids of a copy that are not bound, in the order of its
     * formation.
     *
     * @param value The copy
     * @return The voids
     */
    List<String> opened(final Value value) {
        final List<String> names = new ArrayList<>(0);
        final List<String> order = this.program.form(value.form()).order();
        for (int pos = 0; pos < order.size(); ++pos) {
            if ((value.bound() & 1L << pos) == 0) {
                names.add(order.get(pos));
            }
        }
        return names;
    }

    /**
     * The full name of a value, as the Python engine printed it:
     * {@code formation@birthplace#tag{bound voids}} for a copy.
     *
     * @param num The number
     * @return The name
     */
    String full(final int num) {
        final Value value = this.get(num);
        final String name;
        if (value.kind() == 'O') {
            final StringBuilder text = new StringBuilder(value.form())
                .append('@').append(value.birth());
            if (!value.tag().isEmpty()) {
                text.append('#').append(value.tag());
            }
            name = text.append('{').append(String.join(",", this.bound(value)))
                .append('}').toString();
        } else if (value.kind() == 'B') {
            name = String.format("%s~", this.full(value.base()));
        } else if (value.kind() == 'A') {
            name = String.format("%s/%d", this.full(value.base()), value.count());
        } else if (value.kind() == 'U' || value.kind() == 'X') {
            final StringBuilder text = new StringBuilder().append(value.kind())
                .append(':').append(value.form());
            if (!value.birth().isEmpty()) {
                text.append(':').append(value.birth());
            }
            name = text.toString();
        } else {
            name = String.valueOf(value.kind());
        }
        return name;
    }

    /**
     * The first six values, made once.
     */
    private void start() {
        this.add(new Value('Φ', "", "", 0L, "", -1, 0));
        this.add(new Value('D', "", "", 0L, "", -1, 0));
        this.add(new Value('T', "", "", 0L, "", -1, 0));
        this.add(new Value('J', "", "", 0L, "", -1, 0));
        this.add(new Value('U', "open void", "", 0L, "", -1, 0));
        this.add(new Value('U', "open void", "decided", 0L, "", -1, 0));
    }

    /**
     * Add a value under the next number.
     *
     * @param value The value
     */
    private void add(final Value value) {
        this.index.put(value, this.all.size());
        this.all.add(value);
    }
}
