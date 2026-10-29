/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.BitSet;
import java.util.List;

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
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Places(final State walk) {
        this.state = walk;
    }

    /**
     * The answer at a place in a copy.
     *
     * @param place The place
     * @param ctx The copy around
     * @return The answer
     */
    Ids evaluate(final String place, final int ctx) {
        return this.state.memo.ask(
            new Key('e', ctx, "", place, "", 0),
            () -> this.now(place, ctx)
        );
    }

    /**
     * The answer at a place in a copy, worked out now.
     *
     * @param place The place
     * @param ctx The copy around
     * @return The answer
     * @checkstyle CyclomaticComplexityCheck (100 lines)
     */
    @SuppressWarnings("PMD.CognitiveComplexity")
    private Ints now(final String place, final int ctx) {
        Ints out = new Ints();
        if (this.state.program.formation(place)) {
            final int rho;
            if (this.state.program.form(place).has("ρ")) {
                rho = Copies.NO_RHO;
            } else {
                rho = ctx;
            }
            final int copy = this.state.copies.fresh(
                place, place, rho,
                this.state.tags.tagged(this.state.tags.carried(ctx), place, place)
            );
            this.state.passes.remember(copy);
            out.add(copy);
        } else {
            if (this.state.program.starts(place)) {
                for (final Head head : this.state.program.heads(place)) {
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
            } else if (this.state.program.reads(place)) {
                final Read read = this.state.program.read(place);
                final boolean outside = !this.state.roles.inner(place);
                final String tag = this.state.tags.carried(ctx);
                final Ids got = this.evaluate(read.of(), ctx);
                for (int idx = 0; idx < got.size(); ++idx) {
                    if (outside) {
                        this.state.counting.use(
                            got.get(idx), read.name(), place, tag, Counting.READ, new Ints()
                        );
                    }
                    out.addAll(this.state.reads.attr(got.get(idx), read.name(), place, tag));
                }
            } else {
                throw new IllegalStateException(
                    String.format("%s has no head in the facts", place)
                );
            }
            final List<String> args = this.state.program.args(place);
            if (!args.isEmpty()) {
                out = this.state.calls.apply(out, args, ctx, place);
            }
        }
        return out;
    }

    /**
     * Walk up the {@code ρ} rows from a copy to the copies of a formation.
     *
     * @param place The place of the chain
     * @param loc The formation
     * @param ctx The copy to start from
     * @param out Where the copies found go
     */
    private void outward(final String place, final String loc, final int ctx, final Ints out) {
        final Pile todo = new Pile();
        todo.push(ctx);
        final BitSet met = new BitSet();
        while (!todo.isEmpty()) {
            final int here = todo.pop();
            if (met.get(here)) {
                continue;
            }
            met.set(here);
            final Value value = this.state.values.get(here);
            if (value.kind() == 'O' && value.form().equals(loc)) {
                out.add(here);
                continue;
            }
            if (value.kind() != 'O' || (value.bound() & Formation.RHO) == 0
                || Values.JBORN.equals(value.birth())) {
                throw new IllegalStateException(
                    String.format(
                        "the walk from %s out to %s lost its way at %s",
                        place, loc, this.state.values.full(here)
                    )
                );
            }
            for (final int up : this.state.rows.cell(here, "ρ")) {
                todo.push(up);
            }
        }
    }
}
