/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.List;
import java.util.Map;

/**
 * The copies of the walk (design doc 3): the copies themselves (3.1), what
 * their voids hold (3.2), which of them count (3.3), the study copies (3.4)
 * and the copies made for calls back (3.5).
 *
 * <p>The {@link State} keeps these parts here, so that no class of the walk
 * couples to all of them at once, and passes each call it gets for them on
 * to the part that owns it.</p>
 *
 * @since 0.76.0
 */
final class Population {

    /**
     * The copies (3.1).
     */
    private final Copies copies;

    /**
     * The rows of the voids, and stamps (3.2).
     */
    private final Rows rows;

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
     * Ctor.
     *
     * @param walk The state of the walk
     * @param world The world the walk goes over
     */
    Population(final State walk, final World world) {
        this.copies = new Copies(walk, world);
        this.rows = new Rows(walk, world);
        this.counting = new Counting(walk, world);
        this.study = new Study(walk, world);
        this.callbacks = new Callbacks(walk, world);
    }

    /**
     * Make the shared and the blank copies of the canon, with their rows.
     */
    void seed() {
        this.copies.seed();
    }

    /**
     * A copy of a formation, with these rows gathered into it.
     *
     * @param name The name of the copy
     * @param cells What goes into each void, by void
     * @return The copy
     */
    int copy(final Value name, final Map<String, int[]> cells) {
        return this.copies.copy(name, cells);
    }

    /**
     * A copy with nothing in its voids, and its {@code ρ} only if given.
     *
     * @param form The formation
     * @param born Where it is born, with its tag
     * @param rho The {@code ρ}
     * @return The copy
     */
    int fresh(final String form, final Site born, final int rho) {
        return this.copies.fresh(form, born, rho);
    }

    /**
     * The copy of a formation Java makes, with Java's own {@code ρ}.
     *
     * @param form The formation
     * @return The copy
     */
    int java(final String form) {
        return this.copies.java(form);
    }

    /**
     * The shared copy a blank one joins, or the value itself.
     *
     * @param num The value
     * @return The shared copy, or the value
     */
    int joined(final int num) {
        return this.copies.joined(num);
    }

    /**
     * Whether a formation is a kind of the canon.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean canon(final String form) {
        return this.copies.canon(form);
    }

    /**
     * The shared copy of a kind of the canon.
     *
     * @param form The formation
     * @return The copy
     */
    int shared(final String form) {
        return this.copies.shared(form);
    }

    /**
     * Whether a value is the shared copy of a kind of the canon.
     *
     * @param num The value
     * @return TRUE if it is
     */
    boolean shared(final int num) {
        return this.copies.shared(num);
    }

    /**
     * The bits of every void of a formation, {@code ρ} too.
     *
     * @param form The formation
     * @return The bits
     */
    long mask(final String form) {
        return this.copies.mask(form);
    }

    /**
     * The bits of these voids of a formation.
     *
     * @param form The formation
     * @param names The voids
     * @return The bits
     */
    long bits(final String form, final List<String> names) {
        return this.copies.bits(form, names);
    }

    /**
     * Put a value into a row before the walk starts.
     *
     * @param copy The copy
     * @param name The void
     * @param value The value
     */
    void seed(final int copy, final String name, final int value) {
        this.rows.seed(copy, name, value);
    }

    /**
     * Gather values into a row, which grows.
     *
     * @param copy The copy
     * @param name The void
     * @param values The values
     */
    void gather(final int copy, final String name, final int... values) {
        this.rows.gather(copy, name, values);
    }

    /**
     * What a row holds now, as read by whoever is being asked.
     *
     * @param copy The copy
     * @param name The void
     * @return The values
     */
    int[] cell(final int copy, final String name) {
        return this.rows.cell(copy, name);
    }

    /**
     * A row, read by nobody.
     *
     * @param copy The copy
     * @param name The void
     * @return The row, empty when there is none
     */
    Ints row(final int copy, final String name) {
        return this.rows.row(copy, name);
    }

    /**
     * Every row, by copy and void.
     *
     * @return The rows
     */
    Map<Slot, Ints> table() {
        return this.rows.all();
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
        return this.rows.stamp(item, holder, site);
    }

    /**
     * The tally of changes that may change which copies count.
     *
     * @return The tally
     */
    long tally() {
        return this.counting.tally();
    }

    /**
     * Move the tally.
     */
    void grow() {
        this.counting.grow();
    }

    /**
     * Whether a value is a copy made in EO code with an empty void of its
     * own.
     *
     * @param num The value
     * @return TRUE if it is
     */
    boolean bare(final int num) {
        return this.counting.bare(num);
    }

    /**
     * Whether a value counts by itself.
     *
     * @param num The value
     * @return TRUE if it does
     */
    boolean alone(final int num) {
        return this.counting.alone(num);
    }

    /**
     * Whether a value counts.
     *
     * @param num The value
     * @return TRUE if it does
     */
    boolean counts(final int num) {
        return this.counting.counts(num);
    }

    /**
     * Whether a count told before has turned since.
     *
     * @param told The count, with what it was
     * @return TRUE if it turned
     */
    boolean turned(final Key told) {
        return this.counting.turned(told);
    }

    /**
     * Use a value as it is, down the chain of {@code @}.
     *
     * @param num The value
     * @param usage How it is used, and where
     * @param met The copies met on this chain of {@code @} so far
     */
    void use(final int num, final Usage usage, final Ints met) {
        this.counting.use(num, usage, met);
    }

    /**
     * Note the copy as written of a formation.
     *
     * @param form The formation
     * @param copy Its copy as written
     */
    void write(final String form, final int copy) {
        this.study.write(form, copy);
    }

    /**
     * Whether a stop keeps a copy.
     *
     * @param num The copy
     * @return TRUE if it does
     */
    boolean kept(final int num) {
        return this.study.kept(num);
    }

    /**
     * Stop w: pick the formations no copy calls with a void no copy fills.
     *
     * @return TRUE if it picked any
     */
    boolean pick() {
        return this.study.pick();
    }

    /**
     * Give each formation stop w picked its study copies.
     *
     * @return How many copies were kept
     */
    int studied() {
        return this.study.studied();
    }

    /**
     * Make the copy an atom calls a value back with.
     *
     * @param num The value handed
     * @param types The types the atom calls it back with
     * @param site The site of the call, under the tag of the caller
     */
    void called(final int num, final List<String> types, final Site site) {
        this.callbacks.called(num, types, site);
    }

    /**
     * Note a value handed to an atom.
     *
     * @param num The value
     */
    void handed(final int num) {
        this.callbacks.handed(num);
    }

    /**
     * Whether only Java fills the {@code ρ} of a copy handed to an atom.
     *
     * @param num The copy
     * @return TRUE if only Java does
     */
    boolean handedToJava(final int num) {
        return this.callbacks.handedToJava(num);
    }
}
