/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.IntUnaryOperator;

/**
 * The end step (design doc 4.6): what is decided once after the passes,
 * with nothing fed back.
 *
 * <p>A read with no answer in a copy that counts is marked
 * {@code ?end-can't-tell}, or {@code ?end-nobody-fills-it} when it reads a
 * void of a Java object nobody fills. Then the answers and rows that hold
 * an open void a stop decided are kept aside as they are, and everywhere
 * else that open void becomes a plain one: the answers name a void only
 * Java fills from what was kept aside.</p>
 *
 * @since 0.76.0
 */
final class EndStep {

    /**
     * A read with no answer.
     */
    static final String CANT = "?end-can't-tell";

    /**
     * A read of a void of a Java object nobody fills.
     */
    static final String NOBODY = "?end-nobody-fills-it";

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The mark of each read with no answer, by place and copy.
     */
    private final Map<Slot, String> marks;

    /**
     * The answers kept aside with a decided open void, by question.
     */
    private final Map<Key, Ints> answers;

    /**
     * The rows kept aside with a decided open void, by copy and void.
     */
    private final Map<Slot, Ints> rows;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    EndStep(final State walk) {
        this.state = walk;
        this.marks = new HashMap<>(0);
        this.answers = new HashMap<>(0);
        this.rows = new HashMap<>(0);
    }

    /**
     * Mark the reads with no answer, keep aside what holds a decided open
     * void, and make it a plain one everywhere else.
     */
    void step() {
        this.marked();
        this.keep();
        this.tidied();
    }

    /**
     * The mark of a read with no answer.
     *
     * @param place The place
     * @param ctx The copy
     * @return The mark, empty when there is none
     */
    String mark(final String place, final int ctx) {
        return this.marks.getOrDefault(new Slot(ctx, place), "");
    }

    /**
     * An answer as it was before the end step.
     *
     * @param key The question
     * @param answer The answer now
     * @return The answer it had
     */
    Ints untidy(final Key key, final Ids answer) {
        final Ints out = new Ints();
        if (this.answers.containsKey(key)) {
            out.addAll(this.answers.get(key));
        } else {
            out.addAll(answer);
        }
        return out;
    }

    /**
     * A row as it was before the end step.
     *
     * @param copy The copy
     * @param name The void
     * @return The row it was
     */
    Ints untidy(final int copy, final String name) {
        final Slot slot = new Slot(copy, name);
        final Ints out;
        if (this.rows.containsKey(slot)) {
            out = this.rows.get(slot);
        } else {
            out = this.state.rows().row(copy, name);
        }
        return out;
    }

    private void marked() {
        final Set<List<String>> nobody = this.state.pool().unfilled();
        for (final Key key : this.state.memo().questions()) {
            if (key.kind() != 'e' || !this.state.memo().stored(key).isEmpty()
                || !this.state.counting().counts(key.value())) {
                continue;
            }
            String token = EndStep.CANT;
            if (this.state.program().reads(key.place())) {
                final Read read = this.state.program().read(key.place());
                final Ids got = this.state.memo().stored(
                    new Key('e', key.value(), "", read.of(), "", 0)
                );
                for (int idx = 0; idx < got.size(); ++idx) {
                    final Value value = this.state.values().get(
                        this.state.values().bare(got.get(idx))
                    );
                    if (value.kind() == 'O' && Values.JBORN.equals(value.birth())
                        && nobody.contains(List.of(value.form(), read.name()))) {
                        token = EndStep.NOBODY;
                    }
                }
            }
            this.marks.put(new Slot(key.value(), key.place()), token);
        }
    }

    private void keep() {
        for (final Key key : this.state.memo().questions()) {
            if (key.kind() == 'e' && this.state.memo().stored(key).contains(Values.JGAP)) {
                final Ints got = new Ints();
                got.addAll(this.state.memo().stored(key));
                this.answers.put(key, got);
            }
        }
        for (final Map.Entry<Slot, Ints> row : this.state.rows().all().entrySet()) {
            if (row.getValue().contains(Values.JGAP)) {
                this.rows.put(row.getKey(), row.getValue());
            }
        }
    }

    private void tidied() {
        for (final Key key : this.state.memo().questions()) {
            final Ids got = this.state.memo().stored(key);
            if (key.kind() == 'e' && got.contains(Values.JGAP)) {
                this.state.memo().replace(key, EndStep.tidy(got.size(), got::get));
            }
        }
        for (final Map.Entry<Slot, Ints> row : this.state.rows().all().entrySet()) {
            final Ints got = row.getValue();
            if (got.contains(Values.JGAP)) {
                row.setValue(EndStep.tidy(got.size(), got::get));
            }
        }
        this.state.counting().grow();
    }

    private static Ints tidy(final int size, final IntUnaryOperator values) {
        final Ints out = new Ints();
        for (int idx = 0; idx < size; ++idx) {
            final int val = values.applyAsInt(idx);
            if (val == Values.JGAP) {
                out.add(Values.GAP);
            } else {
                out.add(val);
            }
        }
        return out;
    }
}
