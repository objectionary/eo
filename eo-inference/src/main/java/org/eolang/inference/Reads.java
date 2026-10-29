/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * A name read off a value, at a place, under a tag ({@code attr()}).
 *
 * <ul>
 *   <li>Off {@code Φ}, a formation of that name is a fresh copy at the place,
 *   and a place of that name is its answer.</li>
 *   <li>Off a datum, it fails; off T, it is T; an unknown stays itself.</li>
 *   <li>Off a copy, {@code ρ} is its {@code ρ} row, or a fresh copy of the
 *   declared {@code ^}. A bound void is its row, every value stamped onto
 *   the copy, next to the declared type. A void the copy's name says is
 *   unbound reads T, an open void in a copy a stop kept, its declared type,
 *   or its pool if Java made the copy (4.1, 4.5). A nested formation is a
 *   fresh copy with the copy as {@code ρ}, and any other attribute is its
 *   answer in the copy, stamped onto it.</li>
 *   <li>A name the copy lacks is read off what its {@code @} gives, or off
 *   what its atom returns, or fails when the formation is complete.</li>
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
     * @param place The place of the read
     * @param tag The tag of the reader, empty when none
     * @return What it gives
     */
    Ids attr(final int num, final String name, final String place, final String tag) {
        final int joined = this.state.copies.joined(num);
        return this.state.memo.ask(
            new Key('a', joined, name, place, tag, 0),
            () -> this.now(joined, name, place, tag)
        );
    }

    /**
     * Read a name off a value, worked out now.
     *
     * @param num The value
     * @param name The name
     * @param place The place of the read
     * @param tag The tag of the reader, empty when none
     * @return What it gives
     * @checkstyle CyclomaticComplexityCheck (100 lines)
     * @checkstyle ExecutableStatementCountCheck (100 lines)
     * @checkstyle NestedIfDepthCheck (100 lines)
     */
    @SuppressWarnings({"PMD.CognitiveComplexity", "PMD.NPathComplexity"})
    private Ints now(final int num, final String name, final String place, final String tag) {
        final Value value = this.state.values.get(num);
        final Ints out = new Ints();
        final char kind = value.kind();
        if (kind == 'A' || kind == 'B') {
            out.addAll(this.attr(value.base(), name, place, tag));
        } else if (kind == 'Φ') {
            final String target = String.join(".", "Φ", name);
            if (this.state.program.formation(target)) {
                out.add(
                    this.made(
                        this.state.copies.fresh(
                            target, place, Values.ROOT,
                            this.state.tags.tagged(tag, target, place)
                        )
                    )
                );
            } else if (this.state.program.place(target)) {
                out.addAll(this.state.places.evaluate(target, Values.ROOT));
            } else {
                out.add(this.state.values.conflict(target));
            }
        } else if (kind == 'D') {
            out.add(this.state.values.conflict(String.join(".", "data", name)));
        } else if (kind == 'T') {
            out.add(Values.TERM);
        } else if (kind == 'U') {
            out.add(num);
        } else if (kind == 'O') {
            out.addAll(this.copy(num, value, name, place, tag));
        } else if (kind != 'X') {
            throw new IllegalStateException(
                String.format("%s has nothing to read %s off at %s", this.state.values.full(num), name, place)
            );
        }
        return out;
    }

    /**
     * Read a name off a copy, worked out now.
     *
     * @param num The copy
     * @param value Its value
     * @param name The name
     * @param place The place of the read
     * @param tag The tag of the reader, empty when none
     * @return What it gives
     * @checkstyle ParameterNumberCheck (5 lines)
     * @checkstyle CyclomaticComplexityCheck (100 lines)
     * @checkstyle ExecutableStatementCountCheck (100 lines)
     * @checkstyle NestedIfDepthCheck (100 lines)
     */
    @SuppressWarnings({"PMD.CognitiveComplexity", "PMD.NPathComplexity"})
    private Ints copy(
        final int num, final Value value, final String name, final String place, final String tag
    ) {
        final Ints out = new Ints();
        final String form = value.form();
        final Formation shape = this.state.program.form(form);
        final boolean java = Values.JBORN.equals(value.birth());
        if ("ρ".equals(name)) {
            if ((value.bound() & Formation.RHO) == 0 || java) {
                if (shape.rho().isEmpty()) {
                    throw new IllegalStateException(
                        String.format("%s has no ρ to read at %s", this.state.values.full(num), place)
                    );
                }
                out.add(
                    this.made(
                        this.state.copies.fresh(
                            shape.rho(), place, Copies.NO_RHO,
                            this.state.tags.tagged(tag, shape.rho(), place)
                        )
                    )
                );
            } else {
                for (final int up : this.state.rows.cell(num, "ρ")) {
                    out.add(up);
                }
            }
        } else if (shape.has(name)) {
            final String holds = shape.holds(name);
            if (shape.open(name)) {
                if ((value.bound() & shape.bit(name)) == 0) {
                    if (!holds.isEmpty()) {
                        out.add(this.declared(holds, place, tag));
                    } else if (java && this.state.roles.returned(form)) {
                        out.addAll(this.state.pool.read(num, form, name, place, tag));
                    } else if (this.state.study.kept(num)) {
                        out.add(Values.GAP);
                    } else {
                        out.add(Values.TERM);
                    }
                } else {
                    for (final int held : this.state.rows.cell(num, name)) {
                        out.add(this.made(this.state.rows.stamp(held, num, place, tag)));
                    }
                    if (!holds.isEmpty()) {
                        out.add(this.declared(holds, place, tag));
                    }
                }
            } else {
                final String target = String.join(".", form, name);
                if (this.state.program.formation(target)) {
                    out.add(
                        this.made(
                            this.state.copies.fresh(
                                target, place, num, this.state.tags.tagged(tag, target, place)
                            )
                        )
                    );
                } else {
                    final Ids got = this.state.places.evaluate(target, num);
                    for (int idx = 0; idx < got.size(); ++idx) {
                        out.add(this.made(this.state.rows.stamp(got.get(idx), num, place, tag)));
                    }
                }
            }
        } else if (shape.has("φ")) {
            final Ids got = this.attr(num, "φ", place, tag);
            for (int idx = 0; idx < got.size(); ++idx) {
                out.addAll(this.attr(got.get(idx), name, place, tag));
            }
        } else if (!shape.returns().isEmpty()) {
            final String ret = shape.returns();
            if (this.state.program.formation(ret)) {
                out.addAll(
                    this.attr(
                        this.made(this.state.copies.fresh(ret, Values.JBORN, Values.JAVA, "")),
                        name, place, tag
                    )
                );
            } else if (!this.state.roles.own(form).isEmpty()) {
                for (final String own : this.state.roles.own(form)) {
                    final Ids got = this.attr(num, own, place, tag);
                    for (int idx = 0; idx < got.size(); ++idx) {
                        out.addAll(this.attr(got.get(idx), name, place, tag));
                    }
                }
            } else {
                throw new IllegalStateException(
                    String.format("atom %s returns %s, which it lacks, at %s", form, ret, place)
                );
            }
        } else if (shape.complete()) {
            out.add(this.state.values.conflict(String.join(".", form, name)));
        } else {
            throw new IllegalStateException(
                String.format("atom %s says nothing of %s at %s", form, name, place)
            );
        }
        return out;
    }

    /**
     * A fresh copy of the type a void declares.
     *
     * @param holds The type
     * @param place The place of the read
     * @param tag The tag of the reader, empty when none
     * @return The copy, remembered
     */
    private int declared(final String holds, final String place, final String tag) {
        return this.made(
            this.state.copies.fresh(
                holds, place, Copies.NO_RHO, this.state.tags.tagged(tag, holds, place)
            )
        );
    }

    /**
     * Remember a copy just made, so the passes walk it.
     *
     * @param copy The copy
     * @return The copy
     */
    private int made(final int copy) {
        this.state.passes.remember(copy);
        return copy;
    }
}
