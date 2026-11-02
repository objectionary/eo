/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.List;
import java.util.function.IntConsumer;

/**
 * The ties between the answers of the memo (design doc 6.5): who reads
 * each answer or thing watched, and which answers lost theirs and wait to
 * lose the answers that read them.
 *
 * <p>Each answer and each thing watched is a number, the one the memo gave
 * it, so that the readers of the million answers of eo-runtime fit in
 * arrays. When a thing grows, its readers go stale; before the next cheap
 * pass the stale answers are loosened, and so are the answers that read
 * them, up to the copies walked.</p>
 *
 * @since 0.76.0
 */
final class Ties {

    /**
     * Nobody reads it.
     */
    private static final Ints NOBODY = new Ints();

    /**
     * Who reads each answer or thing watched, by number.
     */
    private final List<Ints> readers;

    /**
     * The answers that lost theirs and wait to lose the answers that read
     * them.
     */
    private final Pile stale;

    /**
     * The marks of {@link #stale}.
     */
    private final BitSet staled;

    /**
     * Ctor.
     */
    Ties() {
        this(new ArrayList<>(0), new Pile(), new BitSet());
    }

    /**
     * Ctor.
     *
     * @param list Who reads each answer, by number
     * @param pile The stale answers
     * @param marks Their marks
     */
    private Ties(final List<Ints> list, final Pile pile, final BitSet marks) {
        this.readers = list;
        this.stale = pile;
        this.staled = marks;
    }

    /**
     * Make room for the next number, which nobody reads yet.
     */
    void grow() {
        this.readers.add(Ties.NOBODY);
    }

    /**
     * Note that one answer reads another, or a thing watched.
     *
     * @param node The answer or thing read
     * @param reader The answer that reads it
     */
    void tie(final int node, final int reader) {
        Ints got = this.readers.get(node);
        if (got.isEmpty()) {
            got = new Ints();
            this.readers.set(node, got);
        }
        got.add(reader);
    }

    /**
     * Whoever read an answer or thing watched goes stale, since it grew.
     *
     * @param node The answer or thing
     */
    void spoil(final int node) {
        final Ints got = this.readers.get(node);
        if (!got.isEmpty()) {
            this.readers.set(node, Ties.NOBODY);
            for (int idx = 0; idx < got.size(); ++idx) {
                this.stale(got.get(idx));
            }
        }
    }

    /**
     * An answer goes stale.
     *
     * @param node The answer
     */
    void stale(final int node) {
        if (!this.staled.get(node)) {
            this.staled.set(node);
            this.stale.push(node);
        }
    }

    /**
     * Loosen the stale answers and every answer that reads them, forgetting
     * who read them.
     *
     * @param lost What to do with each answer loosened, in the order they
     *  come, once or more
     */
    void loosen(final IntConsumer lost) {
        final Pile todo = new Pile();
        for (int idx = 0; idx < this.stale.size(); ++idx) {
            todo.push(this.stale.get(idx));
        }
        this.stale.clear();
        this.staled.clear();
        while (!todo.isEmpty()) {
            final int node = todo.pop();
            lost.accept(node);
            final Ints got = this.readers.get(node);
            this.readers.set(node, Ties.NOBODY);
            for (int idx = 0; idx < got.size(); ++idx) {
                todo.push(got.get(idx));
            }
        }
    }

    /**
     * Forget every tie and every stale answer.
     */
    void clear() {
        Collections.fill(this.readers, Ties.NOBODY);
        this.stale.clear();
        this.staled.clear();
    }
}
