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
 * @checkstyle VisibilityModifierCheck (200 lines)
 * @checkstyle MemberNameCheck (200 lines)
 */
@SuppressWarnings({"PMD.TooManyFields", "PMD.CouplingBetweenObjects"})
final class State {

    /**
     * The program, as {@code facts.xml} states it.
     */
    final Program program;

    /**
     * The names the engine has to know.
     */
    final Names names;

    /**
     * The roles the formations play.
     */
    final Roles roles;

    /**
     * The values (3.1).
     */
    final Values values;

    /**
     * The copies (3.1).
     */
    final Copies copies;

    /**
     * The rows of the voids, and stamps (3.2).
     */
    final Rows rows;

    /**
     * The tags (3.6).
     */
    final Tags tags;

    /**
     * Which copies count (3.3).
     */
    final Counting counting;

    /**
     * The study copies (3.4).
     */
    final Study study;

    /**
     * The copies made for calls back (3.5).
     */
    final Callbacks callbacks;

    /**
     * The memo of answers (6.5).
     */
    final Memo memo;

    /**
     * The reads of a name off a value.
     */
    final Reads reads;

    /**
     * The answers at places.
     */
    final Places places;

    /**
     * The calls.
     */
    final Calls calls;

    /**
     * Reduction (4.2).
     */
    final Reduction reduction;

    /**
     * The rules that wait for a stop (4.3).
     */
    final Rules rules;

    /**
     * The stops (4.4).
     */
    final Stops stops;

    /**
     * The pool (4.5).
     */
    final Pool pool;

    /**
     * The passes (4.1).
     */
    final Passes passes;

    /**
     * The end step (4.6).
     */
    final EndStep end;

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
}
