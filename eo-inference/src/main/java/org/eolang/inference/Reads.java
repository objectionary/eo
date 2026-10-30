/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * A name read off a value, at a place, under a tag ({@code attr()}).
 *
 * <ul>
 * <li>Off {@code Φ}, a formation of that name is a fresh copy at the place,
 * and a place of that name is its answer.</li>
 * <li>Off a datum, it fails; off T, it is T; an unknown stays itself.</li>
 * <li>Off a copy, {@code ρ} is its {@code ρ} row, or a fresh copy of the
 * declared {@code ^}. A bound void is its row, every value stamped onto
 * the copy, next to the declared type. A void the copy's name says is
 * unbound reads T, an open void in a copy a stop kept, its declared type,
 * or its pool if Java made the copy (4.1, 4.5). A nested formation is a
 * fresh copy with the copy as {@code ρ}, and any other attribute is its
 * answer in the copy, stamped onto it.</li>
 * <li>A name the copy lacks is read off what its {@code @} gives, or off
 * what its atom returns, or fails when the formation is complete.</li>
 * </ul>
 *
 * @since 0.76.0
 */
final class Reads {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The world the walk goes over.
     */
    private final World world;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     * @param world The world the walk goes over
     */
    Reads(final State walk, final World world) {
        this.state = walk;
        this.world = world;
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
        final int joined = this.state.joined(num);
        return this.state.ask(
            new Key('a', joined, name, site.place(), site.tag(), 0),
            () -> this.now(joined, name, site)
        );
    }

    private Ints now(final int num, final String name, final Site site) {
        final Value value = this.world.value(num);
        final Ints out = new Ints();
        final char kind = value.kind();
        if (kind == 'A' || kind == 'B') {
            out.addAll(this.attr(value.base(), name, site));
        } else if (kind == 'Φ') {
            out.addAll(this.root(name, site));
        } else if (kind == 'D') {
            out.add(this.world.conflict(String.join(".", "data", name)));
        } else if (kind == 'T') {
            out.add(Values.TERM);
        } else if (kind == 'U') {
            out.add(num);
        } else if (kind == 'O') {
            out.addAll(this.copy(num, name, site));
        } else if (kind != 'X') {
            throw new IllegalStateException(
                String.format(
                    "%s has nothing to read %s off at %s",
                    this.world.full(num), name, site.place()
                )
            );
        }
        return out;
    }

    private Ints root(final String name, final Site site) {
        final String target = String.join(".", "Φ", name);
        final Ints out = new Ints();
        if (this.world.formation(target)) {
            out.add(
                this.made(
                    this.state.fresh(
                        target, this.world.born(site, target), Values.ROOT
                    )
                )
            );
        } else if (this.world.place(target)) {
            out.addAll(this.state.evaluate(target, Values.ROOT));
        } else {
            out.add(this.world.conflict(target));
        }
        return out;
    }

    private Ints copy(final int num, final String name, final Site site) {
        final Value value = this.world.value(num);
        final Formation shape = this.world.form(value.form());
        final Ints out;
        if ("ρ".equals(name)) {
            out = this.rho(num, site);
        } else if (shape.open(name) && (value.bound() & shape.bit(name)) == 0) {
            out = this.unbound(num, name, site);
        } else if (shape.open(name)) {
            out = this.bound(num, name, site);
        } else if (shape.has(name)) {
            out = this.nested(num, name, site);
        } else if (shape.has("φ")) {
            out = this.through(num, "φ", name, site);
        } else if (shape.returns().isEmpty()) {
            out = this.lacking(num, name, site);
        } else {
            out = this.returned(num, name, site);
        }
        return out;
    }

    private Ints rho(final int num, final Site site) {
        final Value value = this.world.value(num);
        final Ints out = new Ints();
        if ((value.bound() & Formation.RHO) == 0 || Values.JBORN.equals(value.birth())) {
            final String rho = this.world.form(value.form()).parent();
            if (rho.isEmpty()) {
                throw new IllegalStateException(
                    String.format(
                        "%s has no ρ to read at %s", this.world.full(num), site.place()
                    )
                );
            }
            out.add(
                this.made(
                    this.state.fresh(
                        rho, this.world.born(site, rho), Copies.NO_RHO
                    )
                )
            );
        } else {
            for (final int outer : this.state.cell(num, "ρ")) {
                out.add(outer);
            }
        }
        return out;
    }

    private Ints unbound(final int num, final String name, final Site site) {
        final String holds = this.world.form(this.world.value(num).form()).holds(name);
        final Ints out;
        if (holds.isEmpty()) {
            out = this.vacant(num, name, site);
        } else {
            out = new Ints(this.declared(holds, site));
        }
        return out;
    }

    private Ints vacant(final int num, final String name, final Site site) {
        final Value value = this.world.value(num);
        final Ints out;
        if (Values.JBORN.equals(value.birth()) && this.world.returned(value.form())) {
            out = this.state.read(num, name, site);
        } else if (this.state.kept(num)) {
            out = new Ints(Values.GAP);
        } else {
            out = new Ints(Values.TERM);
        }
        return out;
    }

    private Ints bound(final int num, final String name, final Site site) {
        final String holds = this.world.form(this.world.value(num).form()).holds(name);
        final Ints out = new Ints();
        for (final int held : this.state.cell(num, name)) {
            out.add(this.made(this.state.stamp(held, num, site)));
        }
        if (!holds.isEmpty()) {
            out.add(this.declared(holds, site));
        }
        return out;
    }

    private Ints nested(final int num, final String name, final Site site) {
        final String target = String.join(".", this.world.value(num).form(), name);
        final Ints out = new Ints();
        if (this.world.formation(target)) {
            out.add(
                this.made(
                    this.state.fresh(target, this.world.born(site, target), num)
                )
            );
        } else {
            final Ids got = this.state.evaluate(target, num);
            for (int idx = 0; idx < got.size(); ++idx) {
                out.add(this.made(this.state.stamp(got.get(idx), num, site)));
            }
        }
        return out;
    }

    private Ints through(final int num, final String via, final String name, final Site site) {
        final Ints out = new Ints();
        final Ids got = this.attr(num, via, site);
        for (int idx = 0; idx < got.size(); ++idx) {
            out.addAll(this.attr(got.get(idx), name, site));
        }
        return out;
    }

    private Ints lacking(final int num, final String name, final Site site) {
        final String form = this.world.value(num).form();
        if (!this.world.form(form).complete()) {
            throw new IllegalStateException(
                String.format("atom %s says nothing of %s at %s", form, name, site.place())
            );
        }
        return new Ints(this.world.conflict(String.join(".", form, name)));
    }

    private Ints returned(final int num, final String name, final Site site) {
        final String form = this.world.value(num).form();
        final String ret = this.world.form(form).returns();
        if (!this.world.formation(ret) && this.world.own(form).isEmpty()) {
            throw new IllegalStateException(
                String.format("atom %s returns %s, which it lacks, at %s", form, ret, site.place())
            );
        }
        final Ints out = new Ints();
        if (this.world.formation(ret)) {
            out.addAll(this.attr(this.made(this.state.java(ret)), name, site));
        } else {
            for (final String own : this.world.own(form)) {
                out.addAll(this.through(num, own, name, site));
            }
        }
        return out;
    }

    private int declared(final String holds, final Site site) {
        return this.made(
            this.state.fresh(holds, this.world.born(site, holds), Copies.NO_RHO)
        );
    }

    private int made(final int copy) {
        this.state.remember(copy);
        return copy;
    }
}
