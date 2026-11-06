/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * What EO code puts into each void of each formation, as {@code facts.xml}
 * says it before any walk (#9657).
 *
 * <p>An atom whose type has no brackets, such as {@code /Q.bool}, leaves
 * the voids of the object Java makes to the EO code that makes such
 * objects: a {@code bool} an atom returns holds in its {@code fork} what
 * {@code true} and {@code false} put there. So every call whose head the
 * facts alone fix to a fresh copy of a formation puts each of its arguments
 * into a void of that formation, where the same call would put it in the
 * walk. An argument is a {@link Filling} the facts fix too when it is a
 * formation written right there, a path that reads a formation from
 * {@code Φ} down through nested formations and bound attributes, a call on
 * a fresh copy of a formation, a datum, a termination or {@code Φ}. Any
 * other argument reads a void, or the copy around, on its way, so only the
 * walk can answer it, and it stands in its void as such. A call on a copy
 * of a formation that is not fresh, such as the copy around, may put an
 * argument into any void from its slot on, and it stands in each of them as
 * one only the walk can answer. A call whose head the facts do not fix puts
 * nothing anywhere.</p>
 *
 * <p>The table is worked out once and from the facts alone, so it does not
 * depend on the order in which the walk meets the files or the
 * objects.</p>
 *
 * @since 0.76.0
 * @todo #9657:60min Decide what a filling only the walk can answer puts
 *  into a void of an object Java makes from a type with no brackets. Such
 *  a filling is a void handed on, as the {@code cde} that
 *  {@code getenv-return} in {@code posix.eo} hands to
 *  {@code posix.getenv-return}. For now {@link Copies} puts the unknown
 *  only Java fills into that void, next to the fillings the facts fix. The
 *  choices are to read such a void as one Java alone fills, to ask for the
 *  type with brackets on every atom whose object such a filling reaches, or
 *  to walk twice, the second time with what the first walk found there.
 */
final class Fillings {

    /**
     * The program, as {@code facts.xml} states it.
     */
    private final Program program;

    /**
     * The values, which tell the void each argument of a call lands in.
     */
    private final Values values;

    /**
     * What goes into each void, by formation and void, worked out once.
     */
    private final Map<Boolean, Map<List<String>, Set<Filling>>> table;

    /**
     * What each place gives before the call there, worked out once.
     */
    private final Map<String, Filling> heads;

    /**
     * Ctor.
     *
     * @param prog The program
     * @param vals The values
     */
    Fillings(final Program prog, final Values vals) {
        this.program = prog;
        this.values = vals;
        this.table = new HashMap<>(1);
        this.heads = new HashMap<>(0);
    }

    /**
     * What EO code puts into a void of a formation.
     *
     * @param form The formation
     * @param name The void
     * @return The fillings, in the order of the facts, none when no call
     *  fills it
     */
    List<Filling> of(final String form, final String name) {
        return new ArrayList<>(
            this.table.computeIfAbsent(true, key -> this.collected())
                .getOrDefault(List.of(form, name), Collections.emptySet())
        );
    }

    private Map<List<String>, Set<Filling>> collected() {
        final Map<List<String>, Set<Filling>> found = new LinkedHashMap<>(0);
        for (final String place : this.program.calls()) {
            final Filling head = this.head(place);
            if (head.kind() == 'O') {
                this.fresh(place, head.loc(), found);
            } else if (head.kind() == 'C' || head.kind() == 'X') {
                this.unnamed(place, head.loc(), found);
            }
        }
        return found;
    }

    private void fresh(
        final String place, final String form, final Map<List<String>, Set<Filling>> found
    ) {
        final List<Argument> args = this.program.args(place);
        final List<String> voids = this.values.voids(
            new Value(form, new Site(place, ""), 0L), args
        );
        for (int idx = 0; idx < voids.size(); ++idx) {
            found.computeIfAbsent(
                List.of(form, voids.get(idx)), key -> new LinkedHashSet<>(1)
            ).add(this.filling(args.get(idx).loc()));
        }
    }

    private void unnamed(
        final String place, final String form, final Map<List<String>, Set<Filling>> found
    ) {
        final List<String> order = this.program.form(form).order();
        for (final Argument arg : this.program.args(place)) {
            final List<String> into;
            if (arg.name().isEmpty()) {
                into = order.subList(Math.min(arg.slot(), order.size()), order.size());
            } else if (order.contains(arg.name())) {
                into = List.of(arg.name());
            } else {
                into = Collections.emptyList();
            }
            for (final String name : into) {
                found.computeIfAbsent(
                    List.of(form, name), key -> new LinkedHashSet<>(1)
                ).add(new Filling('W', arg.loc()));
            }
        }
    }

    private Filling filling(final String place) {
        final Filling got = this.value(place);
        final Filling found;
        if (got.kind() == 'X' || got.kind() == 'W') {
            found = new Filling('W', place);
        } else {
            found = got;
        }
        return found;
    }

    private Filling value(final String place) {
        final Filling head = this.head(place);
        final Filling found;
        if (!this.program.calls(place)) {
            found = head;
        } else if (head.kind() == 'O') {
            found = new Filling('C', head.loc());
        } else {
            found = new Filling('W', place);
        }
        return found;
    }

    private Filling head(final String place) {
        if (!this.heads.containsKey(place)) {
            this.heads.put(place, new Filling('W', place));
            this.heads.put(place, this.started(place));
        }
        return this.heads.get(place);
    }

    private Filling started(final String place) {
        final Filling found;
        if (this.program.formation(place)) {
            found = new Filling('O', place);
        } else if (this.program.starts(place)) {
            found = this.start(place);
        } else if (this.program.reads(place)) {
            final Read read = this.program.read(place);
            found = this.member(this.value(read.of()), read.name());
        } else {
            found = new Filling('W', place);
        }
        return found;
    }

    private Filling start(final String place) {
        final List<Head> starts = this.program.heads(place);
        Filling found = new Filling('W', place);
        if (starts.size() == 1) {
            final Head head = starts.get(0);
            if ("data".equals(head.tag())) {
                found = new Filling('D', "");
            } else if ("terminator".equals(head.tag())) {
                found = new Filling('T', "");
            } else if ("Φ".equals(head.loc())) {
                found = new Filling('Φ', "Φ");
            } else if (this.program.formation(head.loc())) {
                found = new Filling('X', head.loc());
            }
        }
        return found;
    }

    private Filling member(final Filling owner, final String name) {
        final String target = String.join(".", owner.loc(), name);
        final Filling found;
        if ("ΦOCX".indexOf(owner.kind()) < 0) {
            found = new Filling('W', target);
        } else if (this.program.formation(target)) {
            found = new Filling('O', target);
        } else if (owner.kind() == 'Φ' || !this.program.formation(owner.loc())) {
            found = new Filling('W', target);
        } else {
            found = this.attribute(owner.loc(), name);
        }
        return found;
    }

    private Filling attribute(final String form, final String name) {
        final Formation shape = this.program.form(form);
        final String back = new Annotation(shape.returns()).head();
        final Filling found;
        if ("ρ".equals(name) && "Φ".equals(shape.parent())) {
            found = new Filling('Φ', "Φ");
        } else if ("ρ".equals(name) && !shape.parent().isEmpty()) {
            found = new Filling('X', shape.parent());
        } else if (shape.has(name) && !shape.open(name)) {
            found = this.value(String.join(".", form, name));
        } else if (!shape.has(name) && shape.has("φ") && !shape.open("φ")) {
            found = this.member(this.value(String.join(".", form, "φ")), name);
        } else if (!shape.has(name) && this.program.formation(back)) {
            found = this.member(new Filling('X', back), name);
        } else {
            found = new Filling('W', String.join(".", form, name));
        }
        return found;
    }
}
