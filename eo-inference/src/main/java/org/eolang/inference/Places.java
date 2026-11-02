/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.BitSet;

/**
 * The answer at a place in a copy ({@code evaluate()}).
 *
 * <p>A formation written at the place is a fresh copy born there, with the
 * copy around as its {@code ρ} unless it declares its own. A chain starts
 * from a datum, T, {@code Φ}, or the copy of the formation it names, found
 * by walking up the {@code ρ} rows from the copy around. A read reads its
 * name off every value of what it reads it off, and uses each of them, as
 * code reading from outside does (3.3), unless it is the copy's own code. A
 * call then applies what the place gives to its arguments.</p>
 *
 * @since 0.76.0
 */
final class Places {

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
    Places(final State walk, final World world) {
        this.state = walk;
        this.world = world;
    }

    /**
     * The answer at a place in a copy.
     *
     * @param place The place
     * @param ctx The copy around
     * @return The answer
     */
    Ids evaluate(final String place, final int ctx) {
        return this.state.ask(
            new Key('e', ctx, "", place, "", 0),
            () -> this.now(place, ctx)
        );
    }

    private Ints now(final String place, final int ctx) {
        final Ints out;
        if (this.world.formation(place)) {
            out = new Ints(this.formation(place, ctx));
        } else if (this.world.args(place).isEmpty()) {
            out = this.chain(place, ctx);
        } else {
            out = this.state.apply(this.chain(place, ctx), ctx, place);
        }
        return out;
    }

    private int formation(final String place, final int ctx) {
        final int rho;
        if (this.world.form(place).has("ρ")) {
            rho = Copies.NO_RHO;
        } else {
            rho = ctx;
        }
        final int copy = this.state.fresh(
            place,
            this.world.born(new Site(place, this.world.carried(ctx)), place),
            rho
        );
        this.state.remember(copy);
        return copy;
    }

    private Ints chain(final String place, final int ctx) {
        final Ints out;
        if (this.world.starts(place)) {
            out = this.heads(place, ctx);
        } else if (this.world.reads(place)) {
            out = this.read(place, ctx);
        } else {
            throw new IllegalStateException(
                String.format("%s has no head in the facts", place)
            );
        }
        return out;
    }

    private Ints heads(final String place, final int ctx) {
        final Ints out = new Ints();
        for (final Head head : this.world.heads(place)) {
            if ("data".equals(head.tag())) {
                out.add(Values.DATA);
            } else if ("terminator".equals(head.tag())) {
                out.add(Values.TERM);
            } else if ("Φ".equals(head.loc())) {
                out.add(Values.ROOT);
            } else {
                this.outward(place, head.loc(), ctx, out);
            }
        }
        return out;
    }

    private Ints read(final String place, final int ctx) {
        final Read read = this.world.read(place);
        final boolean outside = !this.world.inner(place);
        final Site site = new Site(place, this.world.carried(ctx));
        final Usage usage = new Usage(read.name(), site, Counting.READ);
        final Ints out = new Ints();
        final Ids got = this.evaluate(read.of(), ctx);
        for (int idx = 0; idx < got.size(); ++idx) {
            if (outside) {
                this.state.use(got.get(idx), usage, new Ints());
            }
            out.addAll(this.state.attr(got.get(idx), read.name(), site));
        }
        return out;
    }

    private void outward(final String place, final String loc, final int ctx, final Ints out) {
        final Pile todo = new Pile();
        todo.push(ctx);
        final BitSet met = new BitSet();
        while (!todo.isEmpty()) {
            final int here = todo.pop();
            if (!met.get(here)) {
                met.set(here);
                final Value value = this.world.value(here);
                if (value.kind() == 'O' && value.form().equals(loc)) {
                    out.add(here);
                } else if (value.kind() == 'O' && (value.bound() & Formation.RHO) != 0
                    && !Values.JBORN.equals(value.birth())) {
                    for (final int outer : this.state.cell(here, "ρ")) {
                        todo.push(outer);
                    }
                } else {
                    throw new IllegalStateException(
                        String.format(
                            "the walk from %s out to %s lost its way at %s",
                            place, loc, this.world.full(here)
                        )
                    );
                }
            }
        }
    }
}
