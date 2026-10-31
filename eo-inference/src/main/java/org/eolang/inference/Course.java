/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.List;
import java.util.Set;

/**
 * The course of the walk (design doc 4): the passes (4.1), reduction
 * (4.2), the rules that wait for a stop (4.3), the stops (4.4), the pool
 * (4.5) and the end step (4.6).
 *
 * <p>The {@link State} keeps these parts here, so that no class of the walk
 * couples to all of them at once, and passes each call it gets for them on
 * to the part that owns it.</p>
 *
 * @since 0.76.0
 */
final class Course {

    /**
     * The passes (4.1).
     */
    private final Passes passes;

    /**
     * Reduction (4.2).
     */
    private final Reduction reduction;

    /**
     * The rules that wait for a stop (4.3).
     */
    private final Rules rules;

    /**
     * The stops (4.4).
     */
    private final Stops stops;

    /**
     * The pool (4.5).
     */
    private final Pool pool;

    /**
     * The end step (4.6).
     */
    private final EndStep end;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     * @param world The world the walk goes over
     */
    Course(final State walk, final World world) {
        this.passes = new Passes(walk, world);
        this.reduction = new Reduction(walk, world);
        this.rules = new Rules(walk);
        this.stops = new Stops(walk);
        this.pool = new Pool(walk, world);
        this.end = new EndStep(walk, world);
    }

    /**
     * Remember a value, so the passes walk it.
     *
     * @param num The value
     */
    void remember(final int num) {
        this.passes.remember(num);
    }

    /**
     * Walk a value again in this pass, or for the first time.
     *
     * @param num The value
     */
    void requeue(final int num) {
        this.passes.requeue(num);
    }

    /**
     * Note that something grew.
     */
    void change() {
        this.passes.change();
    }

    /**
     * Whether the passes are over and nothing may be used any more.
     *
     * @return TRUE if they are
     */
    boolean frozen() {
        return this.passes.frozen();
    }

    /**
     * The values remembered, in the order they came.
     *
     * @return The values
     */
    Pile contexts() {
        return this.passes.contexts();
    }

    /**
     * Whether the passes walk a value: it was remembered.
     *
     * @param num The value
     * @return TRUE if it was
     */
    boolean known(final int num) {
        return this.passes.known(num);
    }

    /**
     * How many passes there were.
     *
     * @return The count
     */
    int passes() {
        return this.passes.size();
    }

    /**
     * Mark the copies queued at a stop as walks to do again.
     */
    void queued() {
        this.passes.queued();
    }

    /**
     * Walk until nothing grows and no stop decides anything.
     */
    void settle() {
        this.passes.settle();
    }

    /**
     * What a value reduces to.
     *
     * @param num The value
     * @return The values it stands for
     */
    Ids kind(final int num) {
        return this.reduction.kind(num);
    }

    /**
     * The values of an argument, reduced.
     *
     * @param set The values, as worked out
     * @return The values reduced, in the order of their numbers
     */
    int[] reduced(final int... set) {
        return this.reduction.reduced(set);
    }

    /**
     * A value with rule A's mark: the number of its empty voids.
     *
     * @param base The value, whose own mark is replaced
     * @param count The number of empty voids
     * @return The marked value
     */
    int mark(final int base, final int count) {
        return this.reduction.mark(base, count);
    }

    /**
     * Rule b's head, kept as is.
     *
     * @param head The head
     * @return The value
     */
    int await(final int head) {
        return this.reduction.await(head);
    }

    /**
     * Note that something waits for a stop.
     *
     * @param wish What waits
     */
    void wish(final Wish wish) {
        this.rules.wish(wish);
    }

    /**
     * Whether a stop decided something.
     *
     * @param wish What waited
     * @return TRUE if a stop decided it
     */
    boolean taken(final Wish wish) {
        return this.rules.taken(wish);
    }

    /**
     * What waits now and no stop decided yet.
     *
     * @return What waits
     */
    Set<Wish> wishes() {
        return this.rules.pending();
    }

    /**
     * Decide what waits, for good.
     *
     * @param decided What waits
     */
    void take(final Set<Wish> decided) {
        this.rules.take(decided);
    }

    /**
     * Forget what an answer found waiting, since it lost its answer.
     *
     * @param owner The answer
     */
    void forget(final int owner) {
        this.rules.forget(owner);
    }

    /**
     * Forget everything that waits, before a full pass.
     */
    void clear() {
        this.rules.clear();
    }

    /**
     * Decide at a stop.
     *
     * @return TRUE if anything was decided
     */
    boolean decide() {
        return this.stops.decide();
    }

    /**
     * Whether anything waits for a stop.
     *
     * @return TRUE if something does
     */
    boolean pending() {
        return this.stops.pending();
    }

    /**
     * How many stops there were.
     *
     * @return The count
     */
    int stops() {
        return this.stops.size();
    }

    /**
     * Read an empty void of a copy Java made, from its pool.
     *
     * @param copy The copy
     * @param name The void
     * @param site The site of the read, under the tag of the reader
     * @return What it holds
     */
    Ints read(final int copy, final String name, final Site site) {
        return this.pool.read(copy, name, site);
    }

    /**
     * Put what a call hands to a void of what Java makes into its pool.
     *
     * @param form The formation
     * @param name The void
     * @param values The values
     */
    void fill(final String form, final String name, final int... values) {
        this.pool.fill(form, name, values);
    }

    /**
     * Change a: the voids read with nothing in their pool, not decided yet.
     *
     * @return The voids, as formation and void, in order
     */
    List<List<String>> waiting() {
        return this.pool.waiting();
    }

    /**
     * Decide that Java alone fills these voids.
     *
     * @param pairs The voids, as formation and void
     */
    void decide(final List<List<String>> pairs) {
        this.pool.decide(pairs);
    }

    /**
     * Whether nobody fills a void: it was read with nothing in the pool and
     * never decided.
     *
     * @param form The formation
     * @param name The void
     * @return TRUE if nobody fills it
     */
    boolean unfilled(final String form, final String name) {
        return this.pool.unfilled(form, name);
    }

    /**
     * Mark the reads with no answer, keep aside what holds a decided open
     * void, and make it a plain one everywhere else.
     */
    void step() {
        this.end.step();
    }

    /**
     * The mark of a read with no answer.
     *
     * @param place The place
     * @param ctx The copy
     * @return The mark, empty when there is none
     */
    String token(final String place, final int ctx) {
        return this.end.mark(place, ctx);
    }

    /**
     * An answer as it was before the end step.
     *
     * @param key The question
     * @param answer The answer now
     * @return The answer it had
     */
    Ints untidy(final Key key, final Ids answer) {
        return this.end.untidy(key, answer);
    }

    /**
     * A row as it was before the end step.
     *
     * @param copy The copy
     * @param name The void
     * @return The row it was
     */
    Ints untidy(final int copy, final String name) {
        return this.end.untidy(copy, name);
    }
}
