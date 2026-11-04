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
 * stop decides it, and is kept as itself from then on (rule e). T, rule
 * b's head and every unknown are not typed, and are left out of what an
 * object with empty voids reduces to.</li>
 * <li>Rule b's head goes into a void as what it reduces to, in one
 * step.</li>
 * <li>An unknown goes into a void as it is, so it keeps its kind: the
 * void of an object nobody calls, or a void only Java fills.</li>
 * </ul>
 *
 * @since 0.76.0
 * @todo #9655:30min Remove rule b's head kept as is, the value of kind
 *  {@code 'B'} that {@code await()} makes. A call that waited on unknowns
 *  now makes its copy with them in its voids, so only {@code Rows.stamp()}
 *  calls {@code await()}, and only on a value of that kind, which nothing
 *  makes any more. The kind and every check for it, here and in
 *  {@code Calls}, {@code Reads}, {@code Rows} and {@code Values}, are dead.
 */
final class Reduction {

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
    Reduction(final State walk, final World world) {
        this.state = walk;
        this.world = world;
    }

    /**
     * What a value reduces to.
     *
     * @param num The value
     * @return The values it stands for
     */
    Ids kind(final int num) {
        return this.state.ask(new Key('k', num, "", "", "", 0), () -> this.now(num));
    }

    /**
     * The values of an argument, reduced, each unknown kept as it is.
     *
     * @param set The values, as worked out
     * @return The values reduced, in the order of their numbers
     */
    int[] reduced(final int... set) {
        final Ints all = new Ints();
        for (final int num : set) {
            final Ids kinds = this.kind(num);
            for (int idx = 0; idx < kinds.size(); ++idx) {
                all.addAll(this.unwrapped(kinds.get(idx)));
            }
        }
        final Ids sorted = all.frozen();
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
        return this.mark(base, count, this.world.value(base).origin());
    }

    /**
     * A marked value that keeps the copy whose voids a call must fill.
     *
     * @param base The reduced value
     * @param count The number of empty voids
     * @param origin The source copy, or -1
     * @return The marked value
     */
    int mark(final int base, final int count, final int origin) {
        final Value value = this.world.value(base);
        int inside = base;
        if (value.kind() == 'A') {
            inside = value.base();
        }
        this.state.remember(inside);
        return this.world.intern(new Value('A', "", "", 0L, "", inside, count, origin));
    }

    /**
     * Rule b's head, kept as is.
     *
     * @param head The head
     * @return The value
     */
    int await(final int head) {
        this.state.remember(head);
        return this.world.intern(new Value('B', "", "", 0L, "", head, 0));
    }

    /**
     * A value with rule b's wrapper taken off, as what its head reduces to.
     *
     * @param num The value
     * @return What it stands for
     */
    Ids unwrapped(final int num) {
        final Value value = this.world.value(num);
        final Ids out;
        if (value.kind() == 'B') {
            out = this.kind(value.base());
            for (int idx = 0; idx < out.size(); ++idx) {
                if (this.world.value(out.get(idx)).kind() == 'B') {
                    throw new IllegalStateException(
                        String.format(
                            "%s reduces to another value of rule b",
                            this.world.full(num)
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
        final Value value = this.world.value(num);
        final boolean copy = value.kind() == 'O';
        final Ints result;
        if (copy && this.state.canon(value.form())) {
            result = new Ints(this.state.shared(value.form()));
        } else if (copy && this.world.opened(value).isEmpty()) {
            result = this.closed(num);
        } else if (copy && this.world.decorator(value.form())) {
            result = this.holed(num, this.world.opened(value).size());
        } else {
            result = new Ints(num);
        }
        return result;
    }

    private Ints closed(final int num) {
        final String form = this.world.value(num).form();
        final Formation shape = this.world.form(form);
        final Ints result;
        if (this.world.decorator(form) && this.state.counts(num)) {
            result = this.decorated(num);
        } else if (this.world.decorator(form)
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
        final boolean own = this.state.bare(num);
        final Ints result;
        if (own && !this.state.counts(num)) {
            result = new Ints(num);
        } else if (own) {
            final Ints out = this.phi(num);
            out.remove(num);
            result = new Ints(num);
            final Ints got = this.typed(out);
            if (!got.isEmpty()) {
                result.addAll(this.marked(got, holes, num));
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
        if (got.isEmpty() && (out.isEmpty() || this.state.taken(wish))) {
            result.add(num);
        } else if (got.isEmpty()) {
            this.state.wish(wish);
        } else {
            result.addAll(this.marked(got, holes, -1));
            if (this.state.taken(wish)) {
                result.add(num);
            }
        }
        return result;
    }

    private Ints atom(final int num) {
        final Value value = this.world.value(num);
        final String ret = this.world.form(value.form()).returns();
        final Ints result = new Ints();
        if (this.world.formation(ret)) {
            final int made = this.state.java(ret);
            this.state.remember(made);
            result.add(made);
        } else if (this.world.own(value.form()).isEmpty()) {
            result.add(num);
        } else {
            for (final String own : this.world.own(value.form())) {
                final Ids got = this.state.attr(
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
        final Value value = this.world.value(num);
        final Ints out = new Ints();
        final Ids got = this.state.attr(
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
            final char kind = this.world.value(val).kind();
            if (kind != 'T' && kind != 'B' && kind != 'U') {
                got.add(val);
            }
        }
        return got;
    }

    private Ints marked(final Ints got, final int holes, final int origin) {
        final Ints out = new Ints();
        for (int idx = 0; idx < got.size(); ++idx) {
            out.add(this.mark(got.get(idx), holes, origin));
        }
        return out;
    }
}
