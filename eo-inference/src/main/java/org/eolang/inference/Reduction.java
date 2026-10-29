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
 * <li>Number, string and bytes reduce to their one shared copy.</li>
 * <li>A decorator reduces to what its {@code @} gives; an object met
 * again while it is reduced does not count itself (rule s).</li>
 * <li>A finished atom reduces to what it returns: a new Java copy, or
 * what the own voids it names hold.</li>
 * <li>What an object with empty voids reduces to carries a mark, the
 * number of its empty voids (rule A); a call uses the mark up.</li>
 * <li>An object whose {@code @} gives nothing typed is nothing until a
 * stop decides it, and is kept as itself from then on (rule e).</li>
 * <li>Rule b's head goes into a void as what it reduces to, in one
 * step.</li>
 * <li>Every unknown going into a void is one "can't tell".</li>
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
        return this.state.memo().ask(new Key('k', num, "", "", "", 0), () -> this.now(num));
    }

    /**
     * The values of an argument, reduced: unknowns as one "can't tell".
     *
     * @param set The values, as worked out
     * @return The values reduced, in the order of their numbers
     */
    int[] reduced(final int... set) {
        final int cant = this.state.values().unknown("can't tell");
        final Ints all = new Ints();
        for (final int num : set) {
            final Ids kinds = this.kind(num);
            for (int idx = 0; idx < kinds.size(); ++idx) {
                all.addAll(this.unwrapped(kinds.get(idx)));
            }
        }
        final Ints out = new Ints();
        for (int idx = 0; idx < all.size(); ++idx) {
            if (this.state.values().get(all.get(idx)).kind() == 'U') {
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
        final Value value = this.state.values().get(base);
        int inside = base;
        if (value.kind() == 'A') {
            inside = value.base();
        }
        this.state.passes().remember(inside);
        return this.state.values().intern(new Value('A', "", "", 0L, "", inside, count));
    }

    /**
     * Rule b's head, kept as is.
     *
     * @param head The head
     * @return The value
     */
    int await(final int head) {
        this.state.passes().remember(head);
        return this.state.values().intern(new Value('B', "", "", 0L, "", head, 0));
    }

    /**
     * A value with rule b's wrapper taken off, as what its head reduces to.
     *
     * @param num The value
     * @return What it stands for
     */
    Ids unwrapped(final int num) {
        final Value value = this.state.values().get(num);
        final Ids out;
        if (value.kind() == 'B') {
            out = this.kind(value.base());
            for (int idx = 0; idx < out.size(); ++idx) {
                if (this.state.values().get(out.get(idx)).kind() == 'B') {
                    throw new IllegalStateException(
                        String.format(
                            "%s reduces to another value of rule b",
                            this.state.values().full(num)
                        )
                    );
                }
            }
        } else {
            out = new Ids(num);
        }
        return out;
    }

    private Ints now(final int num) {
        final Value value = this.state.values().get(num);
        final boolean copy = value.kind() == 'O';
        final Ints result;
        if (copy && this.state.copies().canon(value.form())) {
            result = new Ints(this.state.copies().shared(value.form()));
        } else if (copy && this.state.values().opened(value).isEmpty()) {
            result = this.closed(num);
        } else if (copy && this.state.roles().decorator(value.form())) {
            result = this.holed(num, this.state.values().opened(value).size());
        } else {
            result = new Ints(num);
        }
        return result;
    }

    private Ints closed(final int num) {
        final String form = this.state.values().get(num).form();
        final Formation shape = this.state.program().form(form);
        final Ints result;
        if (this.state.roles().decorator(form) && this.state.counting().counts(num)) {
            result = this.decorated(num);
        } else if (this.state.roles().decorator(form)
            || shape.returns().isEmpty() || shape.has("φ")) {
            result = new Ints(num);
        } else {
            result = this.atom(num);
        }
        return result;
    }

    private Ints decorated(final int num) {
        final Ints out = this.phi(num);
        final Ints result = new Ints();
        if (!out.isEmpty()) {
            out.remove(num);
            if (out.isEmpty()) {
                result.add(num);
            } else {
                result.addAll(out);
            }
        }
        return result;
    }

    private Ints holed(final int num, final int holes) {
        final boolean own = this.state.counting().bare(num);
        final Ints result;
        if (own && !this.state.counting().counts(num)) {
            result = new Ints(num);
        } else if (own) {
            final Ints out = this.phi(num);
            out.remove(num);
            result = new Ints(num);
            final Ints got = this.typed(out);
            if (!this.unknowns(got)) {
                result.addAll(this.marked(got, holes));
            }
        } else {
            result = this.inherited(num, holes);
        }
        return result;
    }

    private Ints inherited(final int num, final int holes) {
        final Ints out = this.phi(num);
        final Ints result;
        if (out.isEmpty()) {
            result = new Ints();
        } else {
            out.remove(num);
            result = this.settled(num, out, holes);
        }
        return result;
    }

    private Ints settled(final int num, final Ints out, final int holes) {
        final Ints got = this.typed(out);
        final Wish wish = new Wish('e', "", -1, num);
        final Ints result = new Ints();
        if (this.unknowns(got) && (out.isEmpty() || this.state.rules().taken(wish))) {
            result.add(num);
        } else if (this.unknowns(got)) {
            this.state.rules().wish(wish);
        } else {
            result.addAll(this.marked(got, holes));
            if (this.state.rules().taken(wish)) {
                result.add(num);
            }
        }
        return result;
    }

    private Ints atom(final int num) {
        final Value value = this.state.values().get(num);
        final String ret = this.state.program().form(value.form()).returns();
        final Ints result = new Ints();
        if (this.state.program().formation(ret)) {
            final int made = this.state.copies().java(ret);
            this.state.passes().remember(made);
            result.add(made);
        } else if (this.state.roles().own(value.form()).isEmpty()) {
            result.add(num);
        } else {
            for (final String own : this.state.roles().own(value.form())) {
                final Ids got = this.state.reads().attr(
                    num, own, new Site(String.join(".", value.form(), own), value.tag())
                );
                for (int idx = 0; idx < got.size(); ++idx) {
                    result.addAll(this.kind(got.get(idx)));
                }
            }
        }
        return result;
    }

    private Ints phi(final int num) {
        final Value value = this.state.values().get(num);
        final Ints out = new Ints();
        final Ids got = this.state.reads().attr(
            num, "φ", new Site(String.join(".", value.form(), "φ"), value.tag())
        );
        for (int idx = 0; idx < got.size(); ++idx) {
            out.addAll(this.kind(got.get(idx)));
        }
        return out;
    }

    private Ints typed(final Ints out) {
        final Ints got = new Ints();
        for (int idx = 0; idx < out.size(); ++idx) {
            final int val = out.get(idx);
            final char kind = this.state.values().get(val).kind();
            if (kind != 'T' && kind != 'B' && !this.state.values().gap(val)) {
                got.add(val);
            }
        }
        return got;
    }

    private boolean unknowns(final Ints got) {
        boolean all = true;
        for (int idx = 0; idx < got.size(); ++idx) {
            if (this.state.values().get(got.get(idx)).kind() != 'U') {
                all = false;
                break;
            }
        }
        return all;
    }

    private Ints marked(final Ints got, final int holes) {
        final Ints out = new Ints();
        for (int idx = 0; idx < got.size(); ++idx) {
            final int val = got.get(idx);
            if (this.state.values().get(val).kind() == 'U') {
                out.add(val);
            } else {
                out.add(this.mark(val, holes));
            }
        }
        return out;
    }
}
