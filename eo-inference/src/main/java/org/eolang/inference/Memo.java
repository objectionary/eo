/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.nio.IntBuffer;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;

/**
 * The memo of answers, and who read what (design doc 6.5).
 *
 * <p>Every answer is a stored question, asked again only when it lost its
 * answer. An answer keeps the answers that read it, and so does every row,
 * pool, count and walk of {@code use()}: when one of them grows, the
 * answers that read it lose theirs, then the answers that read those, up to
 * the copies walked, and the next cheap pass works out only those. An
 * answer only grows: what is worked out again is joined to what was
 * there. A question met again while it is being asked gives what it has
 * so far, a cycle hit, and a walk of {@code use()} that met one is not
 * kept as done.</p>
 *
 * <p>Each question is a number here, and so is each thing watched, so that
 * the million answers of eo-runtime, their readers and their marks fit in
 * arrays.</p>
 *
 * @since 0.76.0
 */
final class Memo {

    /**
     * No answer stored yet.
     */
    private static final Ids NONE = new Ids();

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The number of each question and each thing watched.
     */
    private final Map<Key, Integer> nodes;

    /**
     * The questions and things watched, by number.
     */
    private final List<Key> keys;

    /**
     * The answer of each question, by number, {@link #NONE} for none.
     */
    private final List<Ids> answers;

    /**
     * The questions with an answer stored, by number.
     */
    private final BitSet present;

    /**
     * Who reads each answer or thing watched.
     */
    private final Ties ties;

    /**
     * The questions whose answers are good for this pass, and the walks
     * done in it.
     */
    private final BitSet valid;

    /**
     * The questions being asked right now.
     */
    private final BitSet stack;

    /**
     * The counts watched, as their numbers.
     */
    private final BitSet watch;

    /**
     * The questions being asked and the walks being made, one inside the
     * other.
     */
    private final Pile frame;

    /**
     * The questions in the order their answers were first stored.
     */
    private final Pile stored;

    /**
     * Equal answers, as one object, by their values.
     */
    private final Map<IntBuffer, Ids> shared;

    /**
     * How many cycle hits there were.
     */
    private final AtomicLong cycles;

    /**
     * The tally of {@link Counting} the last cheap pass saw.
     */
    private final AtomicLong seen;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Memo(final State walk) {
        this.state = walk;
        this.nodes = new HashMap<>(0);
        this.keys = new ArrayList<>(0);
        this.answers = new ArrayList<>(0);
        this.present = new BitSet();
        this.ties = new Ties();
        this.valid = new BitSet();
        this.stack = new BitSet();
        this.watch = new BitSet();
        this.frame = new Pile();
        this.stored = new Pile();
        this.shared = new HashMap<>(0);
        this.cycles = new AtomicLong();
        this.seen = new AtomicLong(-1L);
    }

    /**
     * Ask a question: its stored answer if it is good, else the answer
     * worked out now and joined to the one stored.
     *
     * @param key The question
     * @param compute How to work it out
     * @return The answer
     */
    Ids ask(final Key key, final Supplier<Ints> compute) {
        final int node = this.node(key);
        final Ids result;
        if (this.valid.get(node)) {
            this.depend(node);
            result = this.answers.get(node);
        } else if (this.stack.get(node)) {
            this.cycles.incrementAndGet();
            this.depend(node);
            result = this.answer(node);
        } else {
            result = this.computed(node, compute);
            this.valid.set(node);
            this.depend(node);
        }
        return result;
    }

    /**
     * The stored answer of a question, asked of nobody.
     *
     * @param key The question
     * @return The answer, none when there is none
     */
    Ids stored(final Key key) {
        final int node = this.find(key);
        final Ids result;
        if (node < 0) {
            result = Memo.NONE;
        } else {
            result = this.answer(node);
        }
        return result;
    }

    /**
     * The questions with stored answers, in the order they were first
     * stored.
     *
     * @return The questions
     */
    List<Key> questions() {
        final List<Key> list = new ArrayList<>(this.stored.size());
        for (int idx = 0; idx < this.stored.size(); ++idx) {
            list.add(this.keys.get(this.stored.get(idx)));
        }
        return list;
    }

    /**
     * Put another answer in place of a stored one, after the passes.
     *
     * @param key The question
     * @param answer The answer
     */
    void replace(final Key key, final Ints answer) {
        final int node = this.find(key);
        this.answers.set(node, this.shared(answer.frozen()));
        this.present.set(node);
    }

    /**
     * Note that whoever is being asked now reads a thing watched.
     *
     * @param key The thing watched
     */
    void depend(final Key key) {
        if (!this.frame.isEmpty()) {
            this.depend(this.node(key));
        }
    }

    /**
     * Whoever read a thing watched loses the answer, since the thing grew.
     *
     * @param key The thing watched
     */
    void spoil(final Key key) {
        final int node = this.find(key);
        if (node >= 0) {
            this.ties.spoil(node);
        }
    }

    /**
     * A question loses its answer before the next cheap pass.
     *
     * @param key The question
     */
    void stale(final Key key) {
        final int node = this.find(key);
        if (node >= 0) {
            this.ties.stale(node);
        }
    }

    /**
     * Watch a count: whoever is being asked now reads it.
     *
     * @param key The count, with what it was
     */
    void watch(final Key key) {
        final int node = this.node(key);
        this.watch.set(node);
        this.depend(node);
    }

    /**
     * Whether a walk is done in this pass.
     *
     * @param key The walk
     * @return TRUE if it is
     */
    boolean done(final Key key) {
        final int node = this.find(key);
        return node >= 0 && this.valid.get(node);
    }

    /**
     * Note a walk as done in this pass.
     *
     * @param key The walk
     */
    void finish(final Key key) {
        this.valid.set(this.node(key));
    }

    /**
     * Start a walk: what is asked from now on is read by it.
     *
     * @param key The walk
     */
    void enter(final Key key) {
        this.frame.push(this.node(key));
    }

    /**
     * End the walk started last.
     */
    void leave() {
        this.frame.pop();
    }

    /**
     * Whether anything is being asked or walked now.
     *
     * @return TRUE if something is
     */
    boolean busy() {
        return !this.frame.isEmpty();
    }

    /**
     * What is being asked or walked now, the innermost.
     *
     * @return Its number
     */
    int owner() {
        return this.frame.top();
    }

    /**
     * How many cycle hits there were so far.
     *
     * @return The count
     */
    long hits() {
        return this.cycles.get();
    }

    /**
     * Forget every mark and reader, before a full pass.
     */
    void clear() {
        this.valid.clear();
        this.ties.clear();
        this.watch.clear();
    }

    /**
     * Forget who reads what, once the passes are over.
     */
    void drop() {
        this.ties.clear();
        this.watch.clear();
    }

    /**
     * Before a cheap pass: the answers that read a count that changed, a
     * copy queued at a stop, or a question a stop decided, lose theirs, and
     * so do the answers that read those.
     */
    void invalidate() {
        final long tally = this.state.tally();
        if (this.seen.get() != tally) {
            this.recount();
            this.seen.set(tally);
        }
        this.state.queued();
        this.ties.loosen(
            node -> {
                this.valid.clear(node);
                this.state.forget(node);
            }
        );
    }

    /**
     * How many questions the memo holds an answer for.
     *
     * @return The count
     */
    int size() {
        return this.stored.size();
    }

    private int node(final Key key) {
        Integer num = this.nodes.get(key);
        if (num == null) {
            num = this.keys.size();
            this.nodes.put(key, num);
            this.keys.add(key);
            this.answers.add(Memo.NONE);
            this.ties.grow();
        }
        return num;
    }

    private int find(final Key key) {
        final Integer num = this.nodes.get(key);
        final int found;
        if (num == null) {
            found = -1;
        } else {
            found = num;
        }
        return found;
    }

    private Ids computed(final int node, final Supplier<Ints> compute) {
        this.stack.set(node);
        this.frame.push(node);
        final Ints got;
        try {
            got = compute.get();
        } finally {
            this.stack.clear(node);
            this.frame.pop();
        }
        final Ids result;
        if (this.present.get(node) && this.answers.get(node).holds(got)) {
            result = this.answers.get(node);
        } else if (this.present.get(node)) {
            result = this.shared(this.answers.get(node).with(got));
            this.answers.set(node, result);
            this.grown(node);
        } else {
            result = this.shared(got.frozen());
            this.answers.set(node, result);
            this.present.set(node);
            this.stored.push(node);
            if (!got.isEmpty()) {
                this.grown(node);
            }
        }
        return result;
    }

    private void grown(final int node) {
        this.state.change();
        this.ties.spoil(node);
    }

    private void recount() {
        final Pile changed = new Pile();
        for (int node = this.watch.nextSetBit(0); node >= 0;
            node = this.watch.nextSetBit(node + 1)) {
            if (this.state.turned(this.keys.get(node))) {
                changed.push(node);
            }
        }
        for (int idx = 0; idx < changed.size(); ++idx) {
            this.watch.clear(changed.get(idx));
            this.ties.spoil(changed.get(idx));
        }
    }

    private Ids answer(final int node) {
        final Ids result;
        if (this.present.get(node)) {
            result = this.answers.get(node);
        } else {
            result = this.shared(new Ids());
        }
        return result;
    }

    private void depend(final int node) {
        if (!this.frame.isEmpty()) {
            this.ties.tie(node, this.frame.top());
        }
    }

    private Ids shared(final Ids got) {
        return got.in(this.shared);
    }
}
