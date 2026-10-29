/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * Reduction (design doc 4.2): what a value stands for, before it goes into
 * a void ({@code kind()}).
 *
 * <ul>
 *   <li>Number, string and bytes reduce to their one shared copy.</li>
 *   <li>A decorator reduces to what its {@code @} gives; an object met
 *   again while it is reduced does not count itself (rule s).</li>
 *   <li>A finished atom reduces to what it returns: a new Java copy, or
 *   what the own voids it names hold.</li>
 *   <li>What an object with empty voids reduces to carries a mark, the
 *   number of its empty voids (rule A); a call uses the mark up.</li>
 *   <li>An object whose {@code @} gives nothing typed is nothing until a
 *   stop decides it, and is kept as itself from then on (rule e).</li>
 *   <li>Rule b's head goes into a void as what it reduces to, in one
 *   step.</li>
 *   <li>Every unknown going into a void is one "can't tell".</li>
 * </ul>
 *
 * @since 0.76.0
 */
final class Reduction {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Reduction(final State walk) {
        this.state = walk;
    }

    /**
     * What a value reduces to.
     *
     * @param num The value
     * @return The values it stands for
     */
    Ids kind(final int num) {
        return this.state.memo.ask(new Key('k', num, "", "", "", 0), () -> this.now(num));
    }

    /**
     * The values of an argument, reduced: unknowns as one "can't tell".
     *
     * @param set The values, as worked out
     * @return The values reduced, in the order of their numbers
     */
    int[] reduced(final int[] set) {
        final int cant = this.state.values.unknown("can't tell");
        final Ints all = new Ints();
        for (final int num : set) {
            final Ids kinds = this.kind(num);
            for (int idx = 0; idx < kinds.size(); ++idx) {
                all.addAll(this.unwrapped(kinds.get(idx)));
            }
        }
        final Ints out = new Ints();
        for (int idx = 0; idx < all.size(); ++idx) {
            if (this.state.values.get(all.get(idx)).kind() == 'U') {
                out.add(cant);
            } else {
                out.add(all.get(idx));
            }
        }
        final Ids sorted = out.frozen();
        final int[] result = new int[sorted.size()];
        for (int idx = 0; idx < result.length; ++idx) {
            result[idx] = sorted.get(idx);
        }
        return result;
    }

    /**
     * A value with rule A's mark: the number of its empty voids.
     *
     * @param base The value, whose own mark is replaced
     * @param count The number of empty voids
     * @return The marked value
     */
    int mark(final int base, final int count) {
        final Value value = this.state.values.get(base);
        int inside = base;
        if (value.kind() == 'A') {
            inside = value.base();
        }
        this.state.passes.remember(inside);
        return this.state.values.intern(new Value('A', "", "", 0L, "", inside, count));
    }

    /**
     * Rule b's head, kept as is.
     *
     * @param head The head
     * @return The value
     */
    int await(final int head) {
        this.state.passes.remember(head);
        return this.state.values.intern(new Value('B', "", "", 0L, "", head, 0));
    }

    /**
     * A value with rule b's wrapper taken off, as what its head reduces to.
     *
     * @param num The value
     * @return What it stands for
     */
    Ids unwrapped(final int num) {
        final Value value = this.state.values.get(num);
        final Ids out;
        if (value.kind() == 'B') {
            out = this.kind(value.base());
            for (int idx = 0; idx < out.size(); ++idx) {
                if (this.state.values.get(out.get(idx)).kind() == 'B') {
                    throw new IllegalStateException(
                        String.format(
                            "%s reduces to another value of rule b",
                            this.state.values.full(num)
                        )
                    );
                }
            }
        } else {
            out = new Ids(num);
        }
        return out;
    }

    /**
     * What a value reduces to, worked out now.
     *
     * @param num The value
     * @return The values it stands for
     * @checkstyle CyclomaticComplexityCheck (100 lines)
     * @checkstyle ExecutableStatementCountCheck (100 lines)
     * @checkstyle NestedIfDepthCheck (100 lines)
     */
    @SuppressWarnings({"PMD.CognitiveComplexity", "PMD.NPathComplexity"})
    private Ints now(final int num) {
        final Value value = this.state.values.get(num);
        final Ints result = new Ints();
        if (value.kind() != 'O') {
            result.add(num);
        } else if (this.state.copies.canon(value.form())) {
            result.add(this.state.copies.shared(value.form()));
        } else {
            final String form = value.form();
            final Formation shape = this.state.program.form(form);
            final int holes = this.state.values.opened(value).size();
            final boolean decor = this.state.roles.decorator(form);
            if (holes > 0) {
                if (!decor) {
                    result.add(num);
                } else {
                    result.addAll(this.holed(num, value, holes));
                }
            } else if (decor) {
                if (this.state.counting.counts(num)) {
                    final Ints out = this.phi(num, value);
                    if (!out.isEmpty()) {
                        out.remove(num);
                        if (out.isEmpty()) {
                            result.add(num);
                        } else {
                            result.addAll(out);
                        }
                    }
                } else {
                    result.add(num);
                }
            } else if (!shape.returns().isEmpty() && !shape.has("φ")) {
                result.addAll(this.atom(num, value, shape));
            } else {
                result.add(num);
            }
        }
        return result;
    }

    /**
     * What a decorator with empty voids reduces to.
     *
     * @param num The value
     * @param value Its value
     * @param holes How many empty voids it has
     * @return The values it stands for
     * @checkstyle CyclomaticComplexityCheck (60 lines)
     */
    @SuppressWarnings("PMD.CognitiveComplexity")
    private Ints holed(final int num, final Value value, final int holes) {
        final Ints result = new Ints();
        final boolean own = this.state.counting.bare(num);
        if (own && !this.state.counting.counts(num)) {
            result.add(num);
        } else {
            final Ints out = this.phi(num, value);
            if (own) {
                out.remove(num);
                final Ints got = this.typed(out);
                if (this.unknowns(got)) {
                    result.add(num);
                } else {
                    result.add(num);
                    result.addAll(this.marked(got, holes));
                }
            } else if (!out.isEmpty()) {
                out.remove(num);
                final Ints got = this.typed(out);
                if (this.unknowns(got)) {
                    if (out.isEmpty()) {
                        result.add(num);
                    } else if (this.state.rules.taken(new Wish('e', "", -1, num))) {
                        result.add(num);
                    } else {
                        this.state.rules.wish(new Wish('e', "", -1, num));
                    }
                } else {
                    result.addAll(this.marked(got, holes));
                    if (this.state.rules.taken(new Wish('e', "", -1, num))) {
                        result.add(num);
                    }
                }
            }
        }
        return result;
    }

    /**
     * What a finished atom reduces to: a new Java copy, or what the own
     * voids it names hold.
     *
     * @param num The value
     * @param value Its value
     * @param shape Its formation
     * @return The values it stands for
     */
    private Ints atom(final int num, final Value value, final Formation shape) {
        final Ints result = new Ints();
        final String ret = shape.returns();
        if (this.state.program.formation(ret)) {
            final int made = this.state.copies.fresh(ret, Values.JBORN, Values.JAVA, "");
            this.state.passes.remember(made);
            result.add(made);
        } else if (this.state.roles.own(value.form()).isEmpty()) {
            result.add(num);
        } else {
            for (final String own : this.state.roles.own(value.form())) {
                final Ids got = this.state.reads.attr(
                    num, own, String.join(".", value.form(), own), value.tag()
                );
                for (int idx = 0; idx < got.size(); ++idx) {
                    result.addAll(this.kind(got.get(idx)));
                }
            }
        }
        return result;
    }

    /**
     * What the {@code @} of a copy reduces to.
     *
     * @param num The value
     * @param value Its value
     * @return The values
     */
    private Ints phi(final int num, final Value value) {
        final Ints out = new Ints();
        final Ids got = this.state.reads.attr(
            num, "φ", String.join(".", value.form(), "φ"), value.tag()
        );
        for (int idx = 0; idx < got.size(); ++idx) {
            out.addAll(this.kind(got.get(idx)));
        }
        return out;
    }

    /**
     * The values of a set that are typed: neither T, rule b's head, nor an
     * open void.
     *
     * @param out The values
     * @return The typed ones
     */
    private Ints typed(final Ints out) {
        final Ints got = new Ints();
        for (int idx = 0; idx < out.size(); ++idx) {
            final int val = out.get(idx);
            final char kind = this.state.values.get(val).kind();
            if (kind != 'T' && kind != 'B' && !this.state.values.gap(val)) {
                got.add(val);
            }
        }
        return got;
    }

    /**
     * Whether every value of a set is an unknown, which an empty set is.
     *
     * @param got The values
     * @return TRUE if every one is
     */
    private boolean unknowns(final Ints got) {
        boolean all = true;
        for (int idx = 0; idx < got.size(); ++idx) {
            if (this.state.values.get(got.get(idx)).kind() != 'U') {
                all = false;
                break;
            }
        }
        return all;
    }

    /**
     * The values of a set, each but an unknown with rule A's mark.
     *
     * @param got The values
     * @param holes How many empty voids the marked object has
     * @return The marked values
     */
    private Ints marked(final Ints got, final int holes) {
        final Ints out = new Ints();
        for (int idx = 0; idx < got.size(); ++idx) {
            final int val = got.get(idx);
            if (this.state.values.get(val).kind() == 'U') {
                out.add(val);
            } else {
                out.add(this.mark(val, holes));
            }
        }
        return out;
    }
}
