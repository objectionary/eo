/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayDeque;
import java.util.BitSet;
import java.util.Deque;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Passes (design doc 4.1): the loop that walks every copy that counts until
 * nothing grows.
 *
 * <p>A full pass works out every answer the run knows, at every place of
 * every copy that counts. A cheap pass works out again only the answers
 * that lost theirs because something they read grew. A pass in which
 * nothing grows is a stop (4.4). The first pass is full, and so is one more
 * after the stop that decides nothing; every other pass is cheap. A copy
 * made while a pass goes is walked in the same pass.</p>
 *
 * <p>Every formation is walked first as written: born at its own place,
 * with its voids empty, inside the copies as written of the formations
 * around it.</p>
 *
 * @since 0.76.0
 */
final class Passes {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The values ever remembered, the copies to walk.
     */
    private final BitSet known;

    /**
     * The values remembered, in the order they came.
     */
    private final Pile contexts;

    /**
     * The copies still to walk in this pass, the last one first.
     */
    private final Pile queue;

    /**
     * Whether anything grew in this pass.
     */
    private final AtomicBoolean changed;

    /**
     * Whether the passes are over.
     */
    private final AtomicBoolean over;

    /**
     * How many passes there were.
     */
    private final AtomicInteger count;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Passes(final State walk) {
        this.state = walk;
        this.known = new BitSet();
        this.contexts = new Pile();
        this.queue = new Pile();
        this.changed = new AtomicBoolean();
        this.over = new AtomicBoolean();
        this.count = new AtomicInteger();
    }

    /**
     * Remember a value, so the passes walk it.
     *
     * @param num The value
     */
    void remember(final int num) {
        if (!this.known.get(num)) {
            this.known.set(num);
            this.contexts.push(num);
            this.queue.push(num);
        }
    }

    /**
     * Walk a value again in this pass, or for the first time.
     *
     * @param num The value
     */
    void requeue(final int num) {
        if (this.known.get(num)) {
            this.queue.push(num);
        } else {
            this.remember(num);
        }
    }

    /**
     * Note that something grew.
     */
    void change() {
        this.changed.set(true);
    }

    /**
     * Whether the passes are over and nothing may be used any more.
     *
     * @return TRUE if they are
     */
    boolean frozen() {
        return this.over.get();
    }

    /**
     * The values remembered, in the order they came.
     *
     * @return The values
     */
    Pile contexts() {
        return this.contexts;
    }

    /**
     * The values remembered, in the order of their numbers.
     *
     * @return The values
     */
    BitSet known() {
        return this.known;
    }

    /**
     * How many passes there were.
     *
     * @return The count
     */
    int size() {
        return this.count.get();
    }

    /**
     * Mark the copies queued at a stop as walks to do again.
     *
     * @param memo The memo
     */
    void queued(final Memo memo) {
        for (int idx = 0; idx < this.queue.size(); ++idx) {
            memo.stale(new Key('w', this.queue.get(idx), "", "", "", 0));
        }
    }

    /**
     * Walk until nothing grows and no stop decides anything.
     *
     * @checkstyle CyclomaticComplexityCheck (60 lines)
     */
    @SuppressWarnings("PMD.CognitiveComplexity")
    void settle() {
        for (final String form : this.state.program.formations()) {
            final Deque<String> chain = new ArrayDeque<>(1);
            String around = form;
            while (!around.isEmpty()) {
                chain.push(around);
                around = this.state.program.nearest(around);
            }
            int rho = Values.ROOT;
            for (final String step : chain) {
                rho = this.state.copies.fresh(step, step, rho, "");
            }
            this.remember(rho);
            this.state.study.write(form, rho);
        }
        this.remember(Values.ROOT);
        boolean whole = true;
        boolean settled = false;
        while (true) {
            this.count.incrementAndGet();
            this.changed.set(false);
            if (whole) {
                this.state.memo.clear();
                this.state.rules.clear();
            } else {
                this.state.memo.invalidate();
            }
            final Pile order = new Pile();
            for (int ctx = this.known.nextSetBit(0); ctx >= 0; ctx = this.known.nextSetBit(ctx + 1)) {
                if (!this.state.memo.done(new Key('w', ctx, "", "", "", 0))) {
                    order.push(ctx);
                }
            }
            this.queue.clear();
            int next = 0;
            while (next < order.size() || !this.queue.isEmpty()) {
                final int ctx;
                if (next < order.size()) {
                    ctx = order.get(next);
                    next += 1;
                } else {
                    ctx = this.queue.pop();
                }
                this.walk(ctx);
            }
            if (this.changed.get()) {
                whole = false;
                settled = false;
                continue;
            }
            if (whole && settled && !this.state.stops.pending()) {
                break;
            }
            settled = !this.state.stops.decide();
            if (whole && settled) {
                break;
            }
            whole = settled;
        }
        this.state.memo.drop();
        this.over.set(true);
    }

    /**
     * Walk one copy: answer every place of its formation in it, and dataize
     * it if it is a test.
     *
     * @param ctx The copy
     */
    private void walk(final int ctx) {
        final Key key = new Key('w', ctx, "", "", "", 0);
        final Value value = this.state.values.get(ctx);
        this.state.memo.enter(key);
        try {
            if (this.state.counting.counts(ctx)) {
                if (value.kind() == 'O') {
                    final List<String> body = this.state.program.body(value.form());
                    for (final String place : body) {
                        this.state.places.evaluate(place, ctx);
                    }
                    if (this.state.roles.test(value.form())) {
                        this.state.counting.use(
                            ctx, "φ", String.join(".", value.form(), "φ"), value.tag(),
                            Counting.TESTED, new Ints()
                        );
                    }
                } else if (ctx == Values.ROOT) {
                    for (final String place : this.state.program.body("")) {
                        this.state.places.evaluate(place, ctx);
                    }
                }
            }
        } finally {
            this.state.memo.leave();
            this.state.memo.finish(key);
        }
    }
}
