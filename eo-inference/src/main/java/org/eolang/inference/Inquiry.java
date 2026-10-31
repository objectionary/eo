/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

/**
 * The questions of the walk and their answers: the answer at a place, a
 * read of a name off a value, a call, and the memo that keeps every answer
 * and who read what (design doc 6.5).
 *
 * <p>The {@link State} keeps these parts here, so that no class of the walk
 * couples to all of them at once, and passes each call it gets for them on
 * to the part that owns it.</p>
 *
 * @since 0.76.0
 */
final class Inquiry {

    /**
     * The memo of answers (6.5).
     */
    private final Memo memo;

    /**
     * The reads of a name off a value.
     */
    private final Reads reads;

    /**
     * The answers at places.
     */
    private final Places places;

    /**
     * The calls.
     */
    private final Calls calls;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     * @param world The world the walk goes over
     */
    Inquiry(final State walk, final World world) {
        this.memo = new Memo(walk);
        this.reads = new Reads(walk, world);
        this.places = new Places(walk, world);
        this.calls = new Calls(walk, world);
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
        return this.memo.ask(key, compute);
    }

    /**
     * The stored answer of a question, asked of nobody.
     *
     * @param key The question
     * @return The answer, none when there is none
     */
    Ids stored(final Key key) {
        return this.memo.stored(key);
    }

    /**
     * The questions with stored answers, in the order they were first
     * stored.
     *
     * @return The questions
     */
    List<Key> questions() {
        return this.memo.questions();
    }

    /**
     * Put another answer in place of a stored one, after the passes.
     *
     * @param key The question
     * @param answer The answer
     */
    void replace(final Key key, final Ints answer) {
        this.memo.replace(key, answer);
    }

    /**
     * Note that whoever is being asked now reads a thing watched.
     *
     * @param key The thing watched
     */
    void depend(final Key key) {
        this.memo.depend(key);
    }

    /**
     * Whoever read a thing watched loses the answer, since the thing grew.
     *
     * @param key The thing watched
     */
    void spoil(final Key key) {
        this.memo.spoil(key);
    }

    /**
     * A question loses its answer before the next cheap pass.
     *
     * @param key The question
     */
    void stale(final Key key) {
        this.memo.stale(key);
    }

    /**
     * Watch a count: whoever is being asked now reads it.
     *
     * @param key The count, with what it was
     */
    void watch(final Key key) {
        this.memo.watch(key);
    }

    /**
     * Whether a walk is done in this pass.
     *
     * @param key The walk
     * @return TRUE if it is
     */
    boolean done(final Key key) {
        return this.memo.done(key);
    }

    /**
     * Note a walk as done in this pass.
     *
     * @param key The walk
     */
    void finish(final Key key) {
        this.memo.finish(key);
    }

    /**
     * Start a walk: what is asked from now on is read by it.
     *
     * @param key The walk
     */
    void enter(final Key key) {
        this.memo.enter(key);
    }

    /**
     * End the walk started last.
     */
    void leave() {
        this.memo.leave();
    }

    /**
     * Whether anything is being asked or walked now.
     *
     * @return TRUE if something is
     */
    boolean busy() {
        return this.memo.busy();
    }

    /**
     * What is being asked or walked now, the innermost.
     *
     * @return Its number
     */
    int owner() {
        return this.memo.owner();
    }

    /**
     * How many cycle hits there were so far.
     *
     * @return The count
     */
    long hits() {
        return this.memo.hits();
    }

    /**
     * Forget every mark and reader, before a full pass.
     */
    void clear() {
        this.memo.clear();
    }

    /**
     * Forget who reads what, once the passes are over.
     */
    void drop() {
        this.memo.drop();
    }

    /**
     * Before a cheap pass, the answers that read what changed lose theirs.
     */
    void invalidate() {
        this.memo.invalidate();
    }

    /**
     * How many questions the memo holds an answer for.
     *
     * @return The count
     */
    int answers() {
        return this.memo.size();
    }

    /**
     * Read a name off a value.
     *
     * @param num The value
     * @param name The name
     * @param site The site of the read, under the tag of the reader
     * @return What it gives
     */
    Ids attr(final int num, final String name, final Site site) {
        return this.reads.attr(num, name, site);
    }

    /**
     * The answer at a place in a copy.
     *
     * @param place The place
     * @param ctx The copy around
     * @return The answer
     */
    Ids evaluate(final String place, final int ctx) {
        return this.places.evaluate(place, ctx);
    }

    /**
     * Apply heads to the arguments of the call at a place.
     *
     * @param heads What the place gives before the call
     * @param ctx The copy around
     * @param place The place of the call
     * @return What the call gives
     */
    Ints apply(final Ints heads, final int ctx, final String place) {
        return this.calls.apply(heads, ctx, place);
    }

    /**
     * Where the arguments of each call land, over the copies around that
     * count.
     *
     * @return The voids, by the place of the argument
     */
    Map<String, String> lands() {
        return this.calls.lands();
    }
}
