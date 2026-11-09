/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.HashMap;
import java.util.Map;

/**
 * The end step (design doc 4.6): what is decided once after the passes,
 * with nothing fed back.
 *
 * <p>First it tells which study copies no longer count (3.4): a study
 * copy counts only if its formation never got a real copy and the copy it
 * was made inside counts. Then a read with no answer in a copy that counts
 * is marked {@code ?end-can't-tell}, told as {@code ?unknown-loop}. A read
 * of a void of a copy Java made always has an answer, what the type of its
 * atom puts there or the unknown only Java fills (4.5), so no such read is
 * marked.</p>
 *
 * @since 0.76.0
 * @todo #9657:30min Remove the mark {@code ?end-can't-tell}, or let telling
 *  name a read with no answer from it. A read of a void Java fills always
 *  has an answer now, so telling names every read with no answer
 *  {@code ?unknown-loop} without a look at its mark, and nothing but
 *  {@code EndStepTest} reads {@code mark()} any more.
 */
final class EndStep {

    /**
     * A read with no answer.
     */
    static final String CANT = "?end-can't-tell";

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The mark of each read with no answer, by place and copy.
     */
    private final Map<Slot, String> marks;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    EndStep(final State walk) {
        this.state = walk;
        this.marks = new HashMap<>(0);
    }

    /**
     * Tell which study copies no longer count, and mark the reads with no
     * answer.
     */
    void step() {
        this.state.conclude();
        for (final Key key : this.state.questions()) {
            if (key.kind() == 'e' && this.state.stored(key).isEmpty()
                && this.state.counts(key.value())) {
                this.marks.put(new Slot(key.value(), key.place()), EndStep.CANT);
            }
        }
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
}
