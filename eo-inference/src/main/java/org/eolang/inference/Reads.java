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
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Reads(final State walk) {
        this.state = walk;
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
        final int joined = this.state.copies().joined(num);
        return this.state.memo().ask(
            new Key('a', joined, name, site.place(), site.tag(), 0),
            () -> this.now(joined, name, site)
        );
    }

    private Ints now(final int num, final String name, final Site site) {
        final Value value = this.state.values().get(num);
        final Ints out = new Ints();
        final char kind = value.kind();
        if (kind == 'A' || kind == 'B') {
            out.addAll(this.attr(value.base(), name, site));
        } else if (kind == 'Φ') {
            out.addAll(this.root(name, site));
        } else if (kind == 'D') {
            out.add(this.state.values().conflict(String.join(".", "data", name)));
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
                    this.state.values().full(num), name, site.place()
                )
            );
        }
        return out;
    }

    private Ints root(final String name, final Site site) {
        final String target = String.join(".", "Φ", name);
        final Ints out = new Ints();
        if (this.state.program().formation(target)) {
            out.add(
                this.made(
                    this.state.copies().fresh(
                        target, this.state.tags().born(site, target), Values.ROOT
                    )
                )
            );
        } else if (this.state.program().place(target)) {
            out.addAll(this.state.places().evaluate(target, Values.ROOT));
        } else {
            out.add(this.state.values().conflict(target));
        }
        return out;
    }

    private Ints copy(final int num, final String name, final Site site) {
        final Value value = this.state.values().get(num);
        final Formation shape = this.state.program().form(value.form());
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
        final Value value = this.state.values().get(num);
        final Ints out = new Ints();
        if ((value.bound() & Formation.RHO) == 0 || Values.JBORN.equals(value.birth())) {
            final String rho = this.state.program().form(value.form()).rho();
            if (rho.isEmpty()) {
                throw new IllegalStateException(
                    String.format(
                        "%s has no ρ to read at %s", this.state.values().full(num), site.place()
                    )
                );
            }
            out.add(
                this.made(
                    this.state.copies().fresh(
                        rho, this.state.tags().born(site, rho), Copies.NO_RHO
                    )
                )
            );
        } else {
            for (final int outer : this.state.rows().cell(num, "ρ")) {
                out.add(outer);
            }
        }
        return out;
    }

    private Ints unbound(final int num, final String name, final Site site) {
        final String holds = this.state.program()
            .form(this.state.values().get(num).form()).holds(name);
        final Ints out;
        if (holds.isEmpty()) {
            out = this.vacant(num, name, site);
        } else {
            out = new Ints(this.declared(holds, site));
        }
        return out;
    }

    private Ints vacant(final int num, final String name, final Site site) {
        final Value value = this.state.values().get(num);
        final Ints out;
        if (Values.JBORN.equals(value.birth()) && this.state.roles().returned(value.form())) {
            out = this.state.pool().read(num, name, site);
        } else if (this.state.study().kept(num)) {
            out = new Ints(Values.GAP);
        } else {
            out = new Ints(Values.TERM);
        }
        return out;
    }

    private Ints bound(final int num, final String name, final Site site) {
        final String holds = this.state.program()
            .form(this.state.values().get(num).form()).holds(name);
        final Ints out = new Ints();
        for (final int held : this.state.rows().cell(num, name)) {
            out.add(this.made(this.state.rows().stamp(held, num, site)));
        }
        if (!holds.isEmpty()) {
            out.add(this.declared(holds, site));
        }
        return out;
    }

    private Ints nested(final int num, final String name, final Site site) {
        final String target = String.join(".", this.state.values().get(num).form(), name);
        final Ints out = new Ints();
        if (this.state.program().formation(target)) {
            out.add(
                this.made(
                    this.state.copies().fresh(target, this.state.tags().born(site, target), num)
                )
            );
        } else {
            final Ids got = this.state.places().evaluate(target, num);
            for (int idx = 0; idx < got.size(); ++idx) {
                out.add(this.made(this.state.rows().stamp(got.get(idx), num, site)));
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
        final String form = this.state.values().get(num).form();
        if (!this.state.program().form(form).complete()) {
            throw new IllegalStateException(
                String.format("atom %s says nothing of %s at %s", form, name, site.place())
            );
        }
        return new Ints(this.state.values().conflict(String.join(".", form, name)));
    }

    private Ints returned(final int num, final String name, final Site site) {
        final String form = this.state.values().get(num).form();
        final String ret = this.state.program().form(form).returns();
        if (!this.state.program().formation(ret) && this.state.roles().own(form).isEmpty()) {
            throw new IllegalStateException(
                String.format("atom %s returns %s, which it lacks, at %s", form, ret, site.place())
            );
        }
        final Ints out = new Ints();
        if (this.state.program().formation(ret)) {
            out.addAll(this.attr(this.made(this.state.copies().java(ret)), name, site));
        } else {
            for (final String own : this.state.roles().own(form)) {
                out.addAll(this.through(num, own, name, site));
            }
        }
        return out;
    }

    private int declared(final String holds, final Site site) {
        return this.made(
            this.state.copies().fresh(holds, this.state.tags().born(site, holds), Copies.NO_RHO)
        );
    }

    private int made(final int copy) {
        this.state.passes().remember(copy);
        return copy;
    }
}
