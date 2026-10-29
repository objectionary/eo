/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;

/**
 * Everything one walk works out and keeps, as the operations its parts ask
 * of each other.
 *
 * <p>The parts call each other the way the sections of the design doc refer
 * to each other: a read (attr) answers through rows and stamps (3.2), makes
 * copies (3.1), counts copies (3.3) and reduces values (4.2), all of it
 * through the memo of answers (6.5), while the passes (4.1) and the stops
 * (4.4) drive the whole. No part reaches another one: each asks the state,
 * and the state passes the call on to the part that owns it. The parts sit
 * in three areas, the copies ({@link Population}), the course of the walk
 * ({@link Course}) and the questions with their memo ({@link Inquiry}), so
 * that no class couples to all of them at once. What never asks the walk
 * back, the program, its roles, the values and their tags, is the
 * {@link World}, which every part asks the same way.</p>
 *
 * @since 0.76.0
 */
final class State {

    /**
     * The copies (3.1 to 3.5).
     */
    private final Population population;

    /**
     * The course of the walk (4.1 to 4.6).
     */
    private final Course course;

    /**
     * The questions and their memo (6.5).
     */
    private final Inquiry inquiry;

    /**
     * Ctor.
     *
     * @param world The world the walk goes over
     */
    State(final World world) {
        this.population = new Population(this, world);
        this.course = new Course(this, world);
        this.inquiry = new Inquiry(this, world);
    }

    /**
     * Make the shared and the blank copies of the canon, with their rows.
     */
    void seed() {
        this.population.seed();
    }

    /**
     * A copy of a formation: the shared or blank one of a kind of the
     * canon, or else the copy of this name, with these rows gathered into
     * it.
     *
     * @param name The name of the copy: its formation, birthplace, tag and
     *  the bits of its bound voids
     * @param rows What goes into each void, by void
     * @return The copy
     */
    int copy(final Value name, final Map<String, int[]> rows) {
        return this.population.copy(name, rows);
    }

    /**
     * A copy with nothing in its voids, and its {@code ρ} only if given:
     * none, Java's own, or a value.
     *
     * @param form The formation
     * @param born Where it is born, with its tag
     * @param rho The {@code ρ}: {@link Copies#NO_RHO}, {@link Values#JAVA}
     *  or a value
     * @return The copy
     */
    int fresh(final String form, final Site born, final int rho) {
        return this.population.fresh(form, born, rho);
    }

    /**
     * The copy of a formation Java makes, with Java's own {@code ρ}.
     *
     * @param form The formation
     * @return The copy
     */
    int java(final String form) {
        return this.population.java(form);
    }

    /**
     * The shared copy a blank one joins, or the value itself.
     *
     * @param num The value
     * @return The shared copy, or the value
     */
    int joined(final int num) {
        return this.population.joined(num);
    }

    /**
     * Whether a formation is a kind of the canon.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean canon(final String form) {
        return this.population.canon(form);
    }

    /**
     * The shared copy of a kind of the canon.
     *
     * @param form The formation
     * @return The copy
     */
    int shared(final String form) {
        return this.population.shared(form);
    }

    /**
     * Whether a value is the shared copy of a kind of the canon.
     *
     * @param num The value
     * @return TRUE if it is
     */
    boolean shared(final int num) {
        return this.population.shared(num);
    }

    /**
     * The bits of every void of a formation, {@code ρ} too.
     *
     * @param form The formation
     * @return The bits
     */
    long mask(final String form) {
        return this.population.mask(form);
    }

    /**
     * The bits of these voids of a formation.
     *
     * @param form The formation
     * @param names The voids
     * @return The bits
     */
    long bits(final String form, final List<String> names) {
        return this.population.bits(form, names);
    }

    /**
     * Put a value into a row before the walk starts.
     *
     * @param copy The copy
     * @param name The void
     * @param value The value
     */
    void seed(final int copy, final String name, final int value) {
        this.population.seed(copy, name, value);
    }

    /**
     * Gather values into a row, which grows.
     *
     * @param copy The copy
     * @param name The void
     * @param values The values
     */
    void gather(final int copy, final String name, final int... values) {
        this.population.gather(copy, name, values);
    }

    /**
     * What a row holds now, as read by whoever is being asked.
     *
     * @param copy The copy
     * @param name The void
     * @return The values
     */
    int[] cell(final int copy, final String name) {
        return this.population.cell(copy, name);
    }

    /**
     * A row, read by nobody.
     *
     * @param copy The copy
     * @param name The void
     * @return The row, empty when there is none
     */
    Ints row(final int copy, final String name) {
        return this.population.row(copy, name);
    }

    /**
     * Every row, by copy and void.
     *
     * @return The rows
     */
    Map<Slot, Ints> table() {
        return this.population.table();
    }

    /**
     * Put a value read out of a row onto its holder.
     *
     * @param item The value
     * @param holder The holder
     * @param site The site of the read, under the tag of the reader
     * @return The value on its holder
     */
    int stamp(final int item, final int holder, final Site site) {
        return this.population.stamp(item, holder, site);
    }

    /**
     * The tally of changes that may change which copies count.
     *
     * @return The tally
     */
    long tally() {
        return this.population.tally();
    }

    /**
     * Move the tally.
     */
    void grow() {
        this.population.grow();
    }

    /**
     * Whether a value is a copy made in EO code with an empty void of its
     * own.
     *
     * @param num The value
     * @return TRUE if it is
     */
    boolean bare(final int num) {
        return this.population.bare(num);
    }

    /**
     * Whether a value counts by itself: it has no empty void of its own, or
     * code uses it, or a stop keeps it.
     *
     * @param num The value
     * @return TRUE if it does
     */
    boolean alone(final int num) {
        return this.population.alone(num);
    }

    /**
     * Whether a value counts.
     *
     * @param num The value
     * @return TRUE if it does
     */
    boolean counts(final int num) {
        return this.population.counts(num);
    }

    /**
     * Whether a count told before has turned since.
     *
     * @param told The count, with what it was
     * @return TRUE if it turned
     */
    boolean turned(final Key told) {
        return this.population.turned(told);
    }

    /**
     * Use a value as it is: read off, dataized by an atom, or dataized by
     * a test, down the chain of {@code @}.
     *
     * @param num The value
     * @param usage How it is used, and where
     * @param met The copies met on this chain of {@code @} so far
     */
    void use(final int num, final Usage usage, final Ints met) {
        this.population.use(num, usage, met);
    }

    /**
     * Note the copy as written of a formation.
     *
     * @param form The formation
     * @param copy Its copy as written
     */
    void write(final String form, final int copy) {
        this.population.write(form, copy);
    }

    /**
     * Whether a stop keeps a copy.
     *
     * @param num The copy
     * @return TRUE if it does
     */
    boolean kept(final int num) {
        return this.population.kept(num);
    }

    /**
     * Stop w: pick the formations no copy calls with a void no copy fills,
     * each with those voids.
     *
     * @return TRUE if it picked any
     */
    boolean pick() {
        return this.population.pick();
    }

    /**
     * Give each formation stop w picked its study copies, or keep its copy
     * as written.
     *
     * @return How many copies were kept
     */
    int studied() {
        return this.population.studied();
    }

    /**
     * Make the copy an atom calls a value back with.
     *
     * @param num The value handed
     * @param types The types the atom calls it back with
     * @param site The site of the call, under the tag of the caller
     */
    void called(final int num, final List<String> types, final Site site) {
        this.population.called(num, types, site);
    }

    /**
     * Note a value handed to an atom.
     *
     * @param num The value
     */
    void handed(final int num) {
        this.population.handed(num);
    }

    /**
     * Whether only Java fills the {@code ρ} of a copy handed to an atom.
     *
     * @param num The copy
     * @return TRUE if only Java does
     */
    boolean handedToJava(final int num) {
        return this.population.handedToJava(num);
    }

    /**
     * Remember a value, so the passes walk it.
     *
     * @param num The value
     */
    void remember(final int num) {
        this.course.remember(num);
    }

    /**
     * Walk a value again in this pass, or for the first time.
     *
     * @param num The value
     */
    void requeue(final int num) {
        this.course.requeue(num);
    }

    /**
     * Note that something grew.
     */
    void change() {
        this.course.change();
    }

    /**
     * Whether the passes are over and nothing may be used any more.
     *
     * @return TRUE if they are
     */
    boolean frozen() {
        return this.course.frozen();
    }

    /**
     * The values remembered, in the order they came.
     *
     * @return The values
     */
    Pile contexts() {
        return this.course.contexts();
    }

    /**
     * Whether the passes walk a value: it was remembered.
     *
     * @param num The value
     * @return TRUE if it was
     */
    boolean known(final int num) {
        return this.course.known(num);
    }

    /**
     * How many passes there were.
     *
     * @return The count
     */
    int passes() {
        return this.course.passes();
    }

    /**
     * Mark the copies queued at a stop as walks to do again.
     */
    void queued() {
        this.course.queued();
    }

    /**
     * Walk until nothing grows and no stop decides anything.
     */
    void settle() {
        this.course.settle();
    }

    /**
     * What a value reduces to.
     *
     * @param num The value
     * @return The values it stands for
     */
    Ids kind(final int num) {
        return this.course.kind(num);
    }

    /**
     * The values of an argument, reduced: unknowns as one "can't tell".
     *
     * @param set The values, as worked out
     * @return The values reduced, in the order of their numbers
     */
    int[] reduced(final int... set) {
        return this.course.reduced(set);
    }

    /**
     * A value with rule A's mark: the number of its empty voids.
     *
     * @param base The value, whose own mark is replaced
     * @param count The number of empty voids
     * @return The marked value
     */
    int mark(final int base, final int count) {
        return this.course.mark(base, count);
    }

    /**
     * Rule b's head, kept as is.
     *
     * @param head The head
     * @return The value
     */
    int await(final int head) {
        return this.course.await(head);
    }

    /**
     * Note that something waits for a stop.
     *
     * @param wish What waits
     */
    void wish(final Wish wish) {
        this.course.wish(wish);
    }

    /**
     * Whether a stop decided something.
     *
     * @param wish What waited
     * @return TRUE if a stop decided it
     */
    boolean taken(final Wish wish) {
        return this.course.taken(wish);
    }

    /**
     * What waits now and no stop decided yet.
     *
     * @return What waits
     */
    Set<Wish> wishes() {
        return this.course.wishes();
    }

    /**
     * Decide what waits, for good.
     *
     * @param decided What waits
     */
    void take(final Set<Wish> decided) {
        this.course.take(decided);
    }

    /**
     * Forget what an answer found waiting, since it lost its answer.
     *
     * @param owner The answer
     */
    void forget(final int owner) {
        this.course.forget(owner);
    }

    /**
     * Forget every mark, reader and wish, before a full pass.
     */
    void clear() {
        this.inquiry.clear();
        this.course.clear();
    }

    /**
     * Decide at a stop.
     *
     * @return TRUE if anything was decided
     */
    boolean decide() {
        return this.course.decide();
    }

    /**
     * Whether anything waits for a stop.
     *
     * @return TRUE if something does
     */
    boolean pending() {
        return this.course.pending();
    }

    /**
     * How many stops there were.
     *
     * @return The count
     */
    int stops() {
        return this.course.stops();
    }

    /**
     * Read an empty void of a copy Java made, from its pool.
     *
     * @param copy The copy
     * @param name The void
     * @param site The site of the read, under the tag of the reader
     * @return What it holds, the decided open void too, nothing while it
     *  waits
     */
    Ints read(final int copy, final String name, final Site site) {
        return this.course.read(copy, name, site);
    }

    /**
     * Put what a call hands to a void of what Java makes into its pool.
     *
     * @param form The formation
     * @param name The void
     * @param values The values
     */
    void fill(final String form, final String name, final int... values) {
        this.course.fill(form, name, values);
    }

    /**
     * Change a: the voids of objects atoms return that were read with
     * nothing in their pool, and are not decided yet.
     *
     * @return The voids, as formation and void, in order
     */
    List<List<String>> waiting() {
        return this.course.waiting();
    }

    /**
     * Decide that Java alone fills these voids.
     *
     * @param pairs The voids, as formation and void
     */
    void decide(final List<List<String>> pairs) {
        this.course.decide(pairs);
    }

    /**
     * Whether nobody fills a void of what Java makes: it was read with
     * nothing in the pool and never decided.
     *
     * @param form The formation
     * @param name The void
     * @return TRUE if nobody fills it
     */
    boolean unfilled(final String form, final String name) {
        return this.course.unfilled(form, name);
    }

    /**
     * Mark the reads with no answer, keep aside what holds a decided open
     * void, and make it a plain one everywhere else.
     */
    void step() {
        this.course.step();
    }

    /**
     * The mark of a read with no answer.
     *
     * @param place The place
     * @param ctx The copy
     * @return The mark, empty when there is none
     */
    String token(final String place, final int ctx) {
        return this.course.token(place, ctx);
    }

    /**
     * An answer as it was before the end step.
     *
     * @param key The question
     * @param answer The answer now
     * @return The answer it had
     */
    Ints untidy(final Key key, final Ids answer) {
        return this.course.untidy(key, answer);
    }

    /**
     * A row as it was before the end step.
     *
     * @param copy The copy
     * @param name The void
     * @return The row it was
     */
    Ints untidy(final int copy, final String name) {
        return this.course.untidy(copy, name);
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
        return this.inquiry.ask(key, compute);
    }

    /**
     * The stored answer of a question, asked of nobody.
     *
     * @param key The question
     * @return The answer, none when there is none
     */
    Ids stored(final Key key) {
        return this.inquiry.stored(key);
    }

    /**
     * The questions with stored answers, in the order they were first
     * stored.
     *
     * @return The questions
     */
    List<Key> questions() {
        return this.inquiry.questions();
    }

    /**
     * Put another answer in place of a stored one, after the passes.
     *
     * @param key The question
     * @param answer The answer
     */
    void replace(final Key key, final Ints answer) {
        this.inquiry.replace(key, answer);
    }

    /**
     * Note that whoever is being asked now reads a thing watched.
     *
     * @param key The thing watched
     */
    void depend(final Key key) {
        this.inquiry.depend(key);
    }

    /**
     * Whoever read a thing watched loses the answer, since the thing grew.
     *
     * @param key The thing watched
     */
    void spoil(final Key key) {
        this.inquiry.spoil(key);
    }

    /**
     * A question loses its answer before the next cheap pass.
     *
     * @param key The question
     */
    void stale(final Key key) {
        this.inquiry.stale(key);
    }

    /**
     * Watch a count: whoever is being asked now reads it.
     *
     * @param key The count, with what it was
     */
    void watch(final Key key) {
        this.inquiry.watch(key);
    }

    /**
     * Whether a walk is done in this pass.
     *
     * @param key The walk
     * @return TRUE if it is
     */
    boolean done(final Key key) {
        return this.inquiry.done(key);
    }

    /**
     * Note a walk as done in this pass.
     *
     * @param key The walk
     */
    void finish(final Key key) {
        this.inquiry.finish(key);
    }

    /**
     * Start a walk: what is asked from now on is read by it.
     *
     * @param key The walk
     */
    void enter(final Key key) {
        this.inquiry.enter(key);
    }

    /**
     * End the walk started last.
     */
    void leave() {
        this.inquiry.leave();
    }

    /**
     * Whether anything is being asked or walked now.
     *
     * @return TRUE if something is
     */
    boolean busy() {
        return this.inquiry.busy();
    }

    /**
     * What is being asked or walked now, the innermost.
     *
     * @return Its number
     */
    int owner() {
        return this.inquiry.owner();
    }

    /**
     * How many cycle hits there were so far.
     *
     * @return The count
     */
    long hits() {
        return this.inquiry.hits();
    }

    /**
     * Forget who reads what, once the passes are over.
     */
    void drop() {
        this.inquiry.drop();
    }

    /**
     * Before a cheap pass: the answers that read a count that changed, a
     * copy queued at a stop, or a question a stop decided, lose theirs, and
     * so do the answers that read those.
     */
    void invalidate() {
        this.inquiry.invalidate();
    }

    /**
     * How many questions the memo holds an answer for.
     *
     * @return The count
     */
    int answers() {
        return this.inquiry.answers();
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
        return this.inquiry.attr(num, name, site);
    }

    /**
     * The answer at a place in a copy.
     *
     * @param place The place
     * @param ctx The copy around
     * @return The answer
     */
    Ids evaluate(final String place, final int ctx) {
        return this.inquiry.evaluate(place, ctx);
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
        return this.inquiry.apply(heads, ctx, place);
    }

    /**
     * Where the arguments of each call land, over the copies around that
     * count: the one void each lands in, for those that land in one.
     *
     * @return The voids, by the place of the argument
     */
    Map<String, String> lands() {
        return this.inquiry.lands();
    }
}
