/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.List;

/**
 * The world one walk goes over: the program as {@code facts.xml} states
 * it, the names the engine has to know, the roles the formations play, the
 * values the walk has made (3.1), and their tags (3.6).
 *
 * <p>Nothing here asks the walk anything back, so the world is made before
 * the {@link State} of the walk, and every part of the walk holds it. Each
 * operation goes to the part that owns it.</p>
 *
 * @since 0.76.0
 */
final class World {

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
     * The tags (3.6).
     */
    private final Tags tags;

    /**
     * Ctor.
     *
     * @param prog The program
     * @param known The names the engine has to know
     */
    World(final Program prog, final Names known) {
        this.program = prog;
        this.names = known;
        this.roles = new Roles(prog);
        this.values = new Values(prog);
        this.tags = new Tags(this);
    }

    /**
     * The names of the formations, in the order of the facts.
     *
     * @return The names
     */
    Collection<String> formations() {
        return this.program.formations();
    }

    /**
     * Whether a name is a formation.
     *
     * @param name The name
     * @return TRUE if it is
     */
    boolean formation(final String name) {
        return this.program.formation(name);
    }

    /**
     * The formation of this name.
     *
     * @param name The name
     * @return The formation
     */
    Formation form(final String name) {
        return this.program.form(name);
    }

    /**
     * Whether a place reads a name.
     *
     * @param place The place
     * @return TRUE if it does
     */
    boolean reads(final String place) {
        return this.program.reads(place);
    }

    /**
     * The read at a place.
     *
     * @param place The place
     * @return The read
     */
    Read read(final String place) {
        return this.program.read(place);
    }

    /**
     * Whether a place starts a chain.
     *
     * @param place The place
     * @return TRUE if it does
     */
    boolean starts(final String place) {
        return this.program.starts(place);
    }

    /**
     * What a chain at a place starts from.
     *
     * @param place The place
     * @return The heads, none when the place starts no chain
     */
    List<Head> heads(final String place) {
        return this.program.heads(place);
    }

    /**
     * The arguments of the call at a place, in order.
     *
     * @param place The place
     * @return The places of the arguments, none when it is not a call
     */
    List<String> args(final String place) {
        return this.program.args(place);
    }

    /**
     * Whether a name is a place the facts answer: a read, a chain or a call.
     *
     * @param name The name
     * @return TRUE if it is
     */
    boolean place(final String name) {
        return this.program.place(name);
    }

    /**
     * The formation right around a place.
     *
     * @param place The place
     * @return The formation, empty when none is around
     */
    String nearest(final String place) {
        return this.program.nearest(place);
    }

    /**
     * The places answered in each copy of a formation, in the order of the
     * facts.
     *
     * @param form The formation, or empty for top-level code
     * @return The places
     */
    List<String> body(final String form) {
        return this.program.body(form);
    }

    /**
     * The canon, the one that holds the data first.
     *
     * @return The names of the formations
     */
    List<String> canon() {
        return this.names.canon();
    }

    /**
     * Whether a package is one of list code.
     *
     * @param pkg The package, as {@code Φ.tuple}
     * @return TRUE if it is
     */
    boolean root(final String pkg) {
        return this.names.root(pkg);
    }

    /**
     * Whether a formation is in a package that takes no tag.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean scalar(final String form) {
        return this.names.scalar(form);
    }

    /**
     * Whether a formation is a decorator.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean decorator(final String form) {
        return this.roles.decorator(form);
    }

    /**
     * Whether a formation is an atom.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean atom(final String form) {
        return this.roles.atom(form);
    }

    /**
     * The own voids an atom gives back, in the order it names them.
     *
     * @param form The formation
     * @return The voids, none when it is no atom or gives back none of its own
     */
    List<String> own(final String form) {
        return this.roles.own(form);
    }

    /**
     * Whether a formation is a test.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean test(final String form) {
        return this.roles.test(form);
    }

    /**
     * The void an object passes on, if its {@code @} is one of its own
     * voids.
     *
     * @param form The formation
     * @return The void, empty when it passes none on
     */
    String passes(final String form) {
        return this.roles.passes(form);
    }

    /**
     * Whether a formation is one an atom returns.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean returned(final String form) {
        return this.roles.returned(form);
    }

    /**
     * The voids of the formations atoms return that have no declared type,
     * as formation and void, in order.
     *
     * @return The pairs
     */
    List<List<String>> javaVoids() {
        return this.roles.javaVoids();
    }

    /**
     * Whether a read is inner: it reads into a formation of the copy's own.
     *
     * @param place The place of the read
     * @return TRUE if it is
     */
    boolean inner(final String place) {
        return this.roles.inner(place);
    }

    /**
     * The number of a value, given now if it has none.
     *
     * @param value The value
     * @return The number
     */
    int intern(final Value value) {
        return this.values.intern(value);
    }

    /**
     * The value of a number.
     *
     * @param num The number
     * @return The value
     */
    Value value(final int num) {
        return this.values.get(num);
    }

    /**
     * How many values there are.
     *
     * @return The count
     */
    int size() {
        return this.values.size();
    }

    /**
     * An unknown, for a reason.
     *
     * @param why The reason
     * @return Its number
     */
    int unknown(final String why) {
        return this.values.unknown(why);
    }

    /**
     * A failing read.
     *
     * @param label What fails
     * @return Its number
     */
    int conflict(final String label) {
        return this.values.conflict(label);
    }

    /**
     * Whether a value is an open void, decided or not.
     *
     * @param num The number
     * @return TRUE if it is
     */
    boolean gap(final int num) {
        return this.values.vacant(num);
    }

    /**
     * The value with the rule b and rule A wrappers taken off.
     *
     * @param num The number
     * @return The number of what is inside
     */
    int bare(final int num) {
        return this.values.bare(num);
    }

    /**
     * The voids of a copy that are bound, {@code ρ} last.
     *
     * @param value The copy
     * @return The voids
     */
    List<String> bound(final Value value) {
        return this.values.bound(value);
    }

    /**
     * The voids of a copy that are not bound.
     *
     * @param value The copy
     * @return The voids
     */
    List<String> opened(final Value value) {
        return this.values.opened(value);
    }

    /**
     * The full name of a value, as the Python engine printed it.
     *
     * @param num The number
     * @return The name
     */
    String full(final int num) {
        return this.values.full(num);
    }

    /**
     * Where a copy of a formation made by code at a site is born: at the
     * same place, under the tag it takes.
     *
     * @param site The site of the call or read
     * @param form The formation
     * @return The site it is born at
     */
    Site born(final Site site, final String form) {
        return this.tags.born(site, form);
    }

    /**
     * The tag a value carries: its own if it is a copy.
     *
     * @param num The value
     * @return The tag, empty when none
     */
    String carried(final int num) {
        return this.tags.carried(num);
    }
}
