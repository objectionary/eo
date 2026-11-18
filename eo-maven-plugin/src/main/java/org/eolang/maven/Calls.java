/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import org.eolang.inference.Annotation;

/**
 * What the calls of the program put into the voids of the copies they make.
 *
 * <p>A call puts its arguments into the voids of the formation its head
 * copies, counting the voids other than {@code ρ} in the order they are
 * written. With {@code [cde msg] > status}, the call {@code status 0 "done"}
 * at {@code Φ.app.φ} puts the object at {@code Φ.app.φ.α0} into
 * {@code cde} and the one at {@code Φ.app.φ.α1} into {@code msg}. An
 * argument bound by name goes into the void it names. The code tells where an
 * argument goes only when its call fills every void of the formation once,
 * so a call with fewer or more arguments than voids tells nothing here. A
 * head whose copy already holds something in a void has no room for that
 * many arguments, so such a call fails before anyone reads its voids.</p>
 *
 * <p>An atom is a call that Java makes. The type it says it returns, such as
 * {@code Φ.posix.status-return(Φ.number,Φ.string)}, names the formation Java
 * copies and lists in brackets what Java puts into each void, in the same
 * order.</p>
 *
 * @since 0.76.0
 */
final class Calls {

    /**
     * The voids of each formation other than {@code ρ}, in the order they
     * are written, by the formation.
     */
    private final Map<String, List<String>> voids;

    /**
     * The arguments each call passes by position: the place of each
     * argument, by its position, by the place of the call.
     */
    private final Map<String, Map<Integer, String>> slots;

    /**
     * The arguments each call binds by name: the place of each argument, by
     * the name of the void it is bound to, by the place of the call.
     */
    private final Map<String, Map<String, String>> names;

    /**
     * The type each atom says it returns, by the atom.
     */
    private final Map<String, Annotation> returns;

    /**
     * The voids each generic atom returns, by the atom.
     *
     * <p>A generic atom, such as {@code [] > recovered /A} with
     * {@code ? > value /A?} and {@code ? > alternative /A}, returns
     * whatever was put into one of its voids that carry the same type
     * variable, and the code does not say which one. Here, the atom
     * {@code Φ.recovered} has two such voids,
     * {@code Φ.recovered.v🌵value} and
     * {@code Φ.recovered.v🌵alternative}.</p>
     */
    private final Map<String, List<String>> generic;

    /**
     * Ctor.
     *
     * @param holes The voids of each formation other than {@code ρ}, in order
     * @param positional The arguments each call passes by position
     * @param named The arguments each call binds by name
     * @param types The type each atom says it returns
     */
    Calls(
        final Map<String, List<String>> holes,
        final Map<String, Map<Integer, String>> positional,
        final Map<String, Map<String, String>> named,
        final Map<String, Annotation> types
    ) {
        this(holes, positional, named, types, Collections.emptyMap());
    }

    /**
     * Ctor.
     *
     * @param holes The voids of each formation other than {@code ρ}, in order
     * @param positional The arguments each call passes by position
     * @param named The arguments each call binds by name
     * @param types The type each atom says it returns
     * @param variable The voids each generic atom returns, by the atom
     */
    Calls(
        final Map<String, List<String>> holes,
        final Map<String, Map<Integer, String>> positional,
        final Map<String, Map<String, String>> named,
        final Map<String, Annotation> types,
        final Map<String, List<String>> variable
    ) {
        this.voids = holes;
        this.slots = positional;
        this.names = named;
        this.returns = types;
        this.generic = variable;
    }

    /**
     * Whether a call is written at the place.
     *
     * @param place The place
     * @return TRUE if it is
     */
    boolean contains(final String place) {
        return this.slots.containsKey(place) || this.names.containsKey(place);
    }

    /**
     * The places of all calls of the program.
     *
     * @return The places
     */
    Set<String> places() {
        final Set<String> all = new TreeSet<>(this.slots.keySet());
        all.addAll(this.names.keySet());
        return all;
    }

    /**
     * The voids of a formation other than {@code ρ}, in the order they are
     * written.
     *
     * @param formation The formation
     * @return The names of the voids, empty when the formation has none
     */
    List<String> voids(final String formation) {
        return this.voids.getOrDefault(formation, Collections.emptyList());
    }

    /**
     * The place of the argument that a call puts into a void.
     *
     * @param call The place of the call
     * @param formation The formation the call copies
     * @param name The name of the void
     * @return The place of the argument, or nothing when the call does not
     *  fill every void of the formation once
     */
    Optional<String> argument(final String call, final String formation, final String name) {
        final List<String> holes = this.voids.getOrDefault(formation, Collections.emptyList());
        final Map<Integer, String> placed = this.slots.getOrDefault(call, Collections.emptyMap());
        final Map<String, String> bound = this.names.getOrDefault(call, Collections.emptyMap());
        final int pos = holes.indexOf(name);
        final Optional<String> found;
        if (pos < 0 || holes.size() != placed.size() + bound.size()) {
            found = Optional.empty();
        } else {
            found = Optional.ofNullable(placed.get(pos)).or(
                () -> Optional.ofNullable(bound.get(name))
            );
        }
        return found;
    }

    /**
     * The type an atom says it returns.
     *
     * @param atom The formation of the atom
     * @return The type, or nothing when the formation is no atom
     */
    Optional<Annotation> returned(final String atom) {
        return Optional.ofNullable(this.returns.get(atom));
    }

    /**
     * The voids a generic atom returns: the places of all its voids that
     * carry the type variable the atom says it returns.
     *
     * @param atom The formation of the atom
     * @return The places of the voids, empty when the atom is not generic
     */
    List<String> slots(final String atom) {
        return this.generic.getOrDefault(atom, Collections.emptyList());
    }

    /**
     * The types with brackets that the atoms say they return, which copy a
     * formation.
     *
     * <p>Only the brackets tell what Java puts into the voids of the copy
     * itself. An atom that returns a type with no brackets, such as
     * {@code Φ.bool}, builds its copy the way {@code Data.ToPhi} does, out of
     * the objects of the program, such as {@code Φ.true}, so the program
     * fills the voids of that copy, not Java.</p>
     *
     * @param formation The formation
     * @return The types, one for each atom that returns a copy of it with
     *  brackets
     */
    List<Annotation> made(final String formation) {
        return this.returns.values().stream().filter(
            type -> type.head().equals(formation) && !type.members().isEmpty()
        ).collect(Collectors.toList());
    }

    /**
     * The type that Java puts into a void of a copy an atom returns.
     *
     * @param type The type the atom says it returns
     * @param name The name of the void
     * @return The type in the brackets, or nothing when they leave the void
     *  out
     */
    Optional<Annotation> member(final Annotation type, final String name) {
        final List<Annotation> members = type.members();
        final int pos = this.voids.getOrDefault(
            type.head(), Collections.emptyList()
        ).indexOf(name);
        final Optional<Annotation> found;
        if (pos >= 0 && pos < members.size()) {
            found = Optional.of(members.get(pos));
        } else {
            found = Optional.empty();
        }
        return found;
    }
}
