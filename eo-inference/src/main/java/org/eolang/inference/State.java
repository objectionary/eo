/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * Everything one walk works out and keeps, in the parts the design doc
 * describes, each part a class that owns its tables.
 *
 * <p>The parts call each other the way the sections of the doc refer to
 * each other: a read (attr) answers through rows and stamps (3.2), makes
 * copies (3.1) with tags (3.6), counts copies (3.3) and reduces values
 * (4.2), all of it through the memo of answers (6.5), while the passes
 * (4.1) and the stops (4.4) drive the whole. The state belongs to the walk
 * alone, and nothing outside it sees a part.</p>
 *
 * @since 0.76.0
 */
final class State {

    /**
     * The program, as {@code facts.xml} states it.
     */
    private final Program program;

    /**
     * The names the engine has to know.
     */
    private final Names names;

    /**
     * The roles the formations play.
     */
    private final Roles roles;

    /**
     * The values (3.1).
     */
    private final Values values;

    /**
     * The copies (3.1).
     */
    private final Copies copies;

    /**
     * The rows of the voids, and stamps (3.2).
     */
    private final Rows rows;

    /**
     * The tags (3.6).
     */
    private final Tags tags;

    /**
     * Which copies count (3.3).
     */
    private final Counting counting;

    /**
     * The study copies (3.4).
     */
    private final Study study;

    /**
     * The copies made for calls back (3.5).
     */
    private final Callbacks callbacks;

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
     * The passes (4.1).
     */
    private final Passes passes;

    /**
     * The end step (4.6).
     */
    private final EndStep end;

    /**
     * Ctor.
     *
     * @param prog The program
     * @param known The names the engine has to know
     */
    State(final Program prog, final Names known) {
        this.program = prog;
        this.names = known;
        this.roles = new Roles(prog);
        this.values = new Values(prog);
        this.copies = new Copies(this);
        this.rows = new Rows(this);
        this.tags = new Tags(this);
        this.counting = new Counting(this);
        this.study = new Study(this);
        this.callbacks = new Callbacks(this);
        this.memo = new Memo(this);
        this.reads = new Reads(this);
        this.places = new Places(this);
        this.calls = new Calls(this);
        this.reduction = new Reduction(this);
        this.rules = new Rules(this);
        this.stops = new Stops(this);
        this.pool = new Pool(this);
        this.passes = new Passes(this);
        this.end = new EndStep(this);
    }

    /**
     * The program, as {@code facts.xml} states it.
     *
     * @return The part
     */
    Program program() {
        return this.program;
    }

    /**
     * The names the engine has to know.
     *
     * @return The part
     */
    Names names() {
        return this.names;
    }

    /**
     * The roles the formations play.
     *
     * @return The part
     */
    Roles roles() {
        return this.roles;
    }

    /**
     * The values (3.1).
     *
     * @return The part
     */
    Values values() {
        return this.values;
    }

    /**
     * The copies (3.1).
     *
     * @return The part
     */
    Copies copies() {
        return this.copies;
    }

    /**
     * The rows of the voids, and stamps (3.2).
     *
     * @return The part
     */
    Rows rows() {
        return this.rows;
    }

    /**
     * The tags (3.6).
     *
     * @return The part
     */
    Tags tags() {
        return this.tags;
    }

    /**
     * Which copies count (3.3).
     *
     * @return The part
     */
    Counting counting() {
        return this.counting;
    }

    /**
     * The study copies (3.4).
     *
     * @return The part
     */
    Study study() {
        return this.study;
    }

    /**
     * The copies made for calls back (3.5).
     *
     * @return The part
     */
    Callbacks callbacks() {
        return this.callbacks;
    }

    /**
     * The memo of answers (6.5).
     *
     * @return The part
     */
    Memo memo() {
        return this.memo;
    }

    /**
     * The reads of a name off a value.
     *
     * @return The part
     */
    Reads reads() {
        return this.reads;
    }

    /**
     * The answers at places.
     *
     * @return The part
     */
    Places places() {
        return this.places;
    }

    /**
     * The calls.
     *
     * @return The part
     */
    Calls calls() {
        return this.calls;
    }

    /**
     * Reduction (4.2).
     *
     * @return The part
     */
    Reduction reduction() {
        return this.reduction;
    }

    /**
     * The rules that wait for a stop (4.3).
     *
     * @return The part
     */
    Rules rules() {
        return this.rules;
    }

    /**
     * The stops (4.4).
     *
     * @return The part
     */
    Stops stops() {
        return this.stops;
    }

    /**
     * The pool (4.5).
     *
     * @return The part
     */
    Pool pool() {
        return this.pool;
    }

    /**
     * The passes (4.1).
     *
     * @return The part
     */
    Passes passes() {
        return this.passes;
    }

    /**
     * The end step (4.6).
     *
     * @return The part
     */
    EndStep end() {
        return this.end;
    }
}
