/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * The copies (design doc 3.1): one use of a formation, with its own values
 * in its voids.
 *
 * <p>A copy is named by its formation, its birthplace, its tag and the set
 * of its bound voids, so two calls written at one place under one tag share
 * one copy, and their values meet in its rows. Number, string and bytes are
 * one shared copy each, born {@code canon}: bytes holds the data and the
 * other two hold bytes. Each of them also has a blank copy for the bare
 * name, born {@code blank}, which joins the shared one wherever it is put
 * into a {@code ρ}. An object an atom returns is one copy per option of
 * its type (an {@link Annotation}), born {@code java}, with the bracket list
 * of that option right after it, so two types that put different things
 * into the voids of one formation make two copies. The i-th type in the
 * brackets types the i-th void of the copy, which holds the copies Java
 * makes of that type in turn, one for each option of a union, and a void the
 * brackets leave out stays empty. A kind of the canon is its shared copy,
 * whatever its brackets say.</p>
 *
 * <p>A copy has a {@code ρ} only when its formation declares {@code ^}, as
 * in the runtime since #6985: a {@code ρ} handed to a copy of a formation
 * that declares none is dropped, and neither its name nor its rows keep it
 * (#9785). The shared and the blank copies of a kind of the canon that
 * declares no {@code ^} have no {@code ρ} either, and the bare name is
 * still told from the shared copy by the {@code Φ} it is handed.</p>
 *
 * @since 0.76.0
 * @todo #9657:60min Let a type variable in the brackets of a type or in a
 *  call back list, as in {@code Φ.box(A)} or {@code /{Q.string B}}, stand
 *  for what the voids of the atom that carry the same letter hold, the way
 *  {@code /A} does for the whole return of {@code recovered}. The copy
 *  Java makes would then depend on the copy of the atom it comes from, so
 *  it could no longer be one copy per type, and for now {@code java()}
 *  stops the engine on such a type. Nothing in eo-runtime writes one yet.
 */
final class Copies {

    /**
     * No {@code ρ} to put into a fresh copy.
     */
    static final int NO_RHO = -1;

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The world the walk goes over.
     */
    private final World world;

    /**
     * The shared copy of each kind of the canon.
     */
    private final Map<String, Integer> canon;

    /**
     * The blank copy of each kind of the canon.
     */
    private final Map<String, Integer> blank;

    /**
     * The blank copies, then the shared ones they join, in the same order.
     */
    private final Ints joins;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     * @param world The world the walk goes over
     */
    Copies(final State walk, final World world) {
        this.state = walk;
        this.world = world;
        this.canon = new LinkedHashMap<>(0);
        this.blank = new LinkedHashMap<>(0);
        this.joins = new Ints();
    }

    /**
     * Make the shared and the blank copies of the canon, with their rows.
     *
     * <p>A program that lacks a kind of the canon has no canon at all, and
     * every kind of it is a formation like any other: a program of a few
     * files of its own, with no {@code number.eo}, is walked all the
     * same.</p>
     */
    void seed() {
        final List<String> kinds = this.world.canon();
        boolean all = !kinds.isEmpty();
        for (final String kind : kinds) {
            if (!this.world.formation(kind)) {
                all = false;
                break;
            }
        }
        if (all) {
            this.share(kinds);
        }
    }

    /**
     * A copy of a formation: the shared or blank one of a kind of the
     * canon, or else the copy of this name, with these rows gathered into
     * it.
     *
     * @param name The name of the copy: its formation, birthplace, tag and
     *  the bits of its bound voids
     * @param rows What goes into each void, by void
     * @return The copy
     */
    int copy(final Value name, final Map<String, int[]> rows) {
        final String form = name.form();
        final int made;
        if (this.canon.containsKey(form)) {
            if (!name.java() && (name.bound() & ~Formation.RHO) == 0
                && Copies.rooted(rows.getOrDefault("ρ", new int[0]))) {
                made = this.blank.get(form);
            } else {
                made = this.canon.get(form);
            }
        } else {
            final long mask = this.mask(form);
            made = this.world.intern(
                new Value('O', form, name.birth(), name.bound() & mask, name.tag(), -1, 0)
            );
            for (final Map.Entry<String, int[]> row : rows.entrySet()) {
                if (!"ρ".equals(row.getKey()) || (mask & Formation.RHO) != 0) {
                    this.state.gather(made, row.getKey(), row.getValue());
                }
            }
        }
        return made;
    }

    /**
     * A copy with nothing in its voids, and its {@code ρ} only if given:
     * none, Java's own, or a value, which it keeps only when its formation
     * declares {@code ^}. A formation with no name of its own that declares
     * {@code ^} takes as its {@code ^} the object it goes into and nothing
     * else, so unless Java makes it, its copy has that {@code ^} bound and
     * empty whatever is given, and the call that puts it into a void fills
     * it (#9655).
     *
     * @param form The formation
     * @param born Where it is born, with its tag
     * @param rho The {@code ρ}: {@link #NO_RHO}, {@link Values#JAVA} or a value
     * @return The copy
     */
    int fresh(final String form, final Site born, final int rho) {
        final int made;
        if (rho == Values.JAVA) {
            made = this.java(new Annotation(form));
        } else if (this.world.form(form).applied()) {
            made = this.copy(new Value(form, born, Formation.RHO), Map.of());
        } else if (rho == Copies.NO_RHO) {
            made = this.copy(new Value(form, born, 0L), Map.of());
        } else {
            made = this.copy(new Value(form, born, Formation.RHO), Map.of("ρ", new int[] {rho}));
        }
        return made;
    }

    /**
     * The copy Java makes of one option of a type, with Java's own
     * {@code ρ} and its voids typed by the brackets of the option.
     *
     * @param option The option, a path with its brackets
     * @return The copy
     */
    int java(final Annotation option) {
        if (option.variable()) {
            throw new IllegalStateException(
                String.format(
                    "Java makes a copy of the type variable %s, which the engine cannot follow outside the return of an atom yet",
                    option
                )
            );
        }
        if (!this.world.formation(option.head())) {
            throw new IllegalStateException(
                String.format(
                    "Java makes a copy of %s, which is not a formation of the program",
                    option.head()
                )
            );
        }
        return this.typed(
            option,
            new Site(
                Values.JBORN.concat(option.toString().substring(option.head().length())), ""
            ),
            Formation.RHO
        );
    }

    /**
     * The copy of one option of the type a void declares, born at a read of
     * that void, with its voids typed by the brackets of the option.
     *
     * @param option The option, a path with its brackets
     * @param born Where it is born, with its tag
     * @return The copy
     */
    int declared(final Annotation option, final Site born) {
        return this.typed(option, born, 0L);
    }

    /**
     * The shared copy a blank one joins, or the value itself.
     *
     * @param num The value
     * @return The shared copy, or the value
     */
    int joined(final int num) {
        int found = num;
        final int half = this.joins.size() / 2;
        for (int idx = 0; idx < half; ++idx) {
            if (this.joins.get(idx) == num) {
                found = this.joins.get(idx + half);
                break;
            }
        }
        return found;
    }

    /**
     * Whether a formation is a kind of the canon.
     *
     * @param form The formation
     * @return TRUE if it is
     */
    boolean canon(final String form) {
        return this.canon.containsKey(form);
    }

    /**
     * The shared copy of a kind of the canon.
     *
     * @param form The formation
     * @return The copy
     */
    int shared(final String form) {
        return this.canon.get(form);
    }

    /**
     * Whether a value is the shared copy of a kind of the canon.
     *
     * @param num The value
     * @return TRUE if it is
     */
    boolean shared(final int num) {
        return this.canon.containsValue(num);
    }

    /**
     * The bits of every void of a formation, {@code ρ} too when it declares
     * {@code ^}.
     *
     * @param form The formation
     * @return The bits
     */
    long mask(final String form) {
        final Formation shape = this.world.form(form);
        long mask = (1L << shape.order().size()) - 1L;
        if (shape.open("ρ")) {
            mask |= Formation.RHO;
        }
        return mask;
    }

    /**
     * The bits of these voids of a formation.
     *
     * @param form The formation
     * @param names The voids
     * @return The bits
     */
    long bits(final String form, final List<String> names) {
        final Formation shape = this.world.form(form);
        long bits = 0L;
        for (final String name : names) {
            bits |= shape.bit(name);
        }
        return bits;
    }

    private void share(final List<String> kinds) {
        for (final String kind : kinds) {
            if (!List.of("φ").equals(this.world.form(kind).order())) {
                throw new IllegalStateException(
                    String.format("%s must have one void, φ, to be one of the canon", kind)
                );
            }
            this.canon.put(
                kind,
                this.world.intern(new Value('O', kind, "canon", this.mask(kind), "", -1, 0))
            );
        }
        final int datum = this.canon.get(kinds.get(0));
        for (final String kind : kinds) {
            final int shared = this.canon.get(kind);
            if (shared == datum) {
                this.state.seed(shared, "φ", Values.DATA);
            } else {
                this.state.seed(shared, "φ", datum);
            }
            if (this.world.form(kind).open("ρ")) {
                this.state.seed(shared, "ρ", Values.ROOT);
            }
        }
        for (final String kind : kinds) {
            final int empty = this.world.intern(
                new Value('O', kind, "blank", this.mask(kind) & Formation.RHO, "", -1, 0)
            );
            this.blank.put(kind, empty);
            if (this.world.form(kind).open("ρ")) {
                this.state.seed(empty, "ρ", Values.ROOT);
            }
        }
        for (final String kind : kinds) {
            this.joins.add(this.blank.get(kind));
        }
        for (final String kind : kinds) {
            this.joins.add(this.canon.get(kind));
        }
    }

    private int typed(final Annotation option, final Site born, final long bits) {
        final String form = option.head();
        final Map<String, int[]> rows = new LinkedHashMap<>(0);
        if (!this.canon.containsKey(form)) {
            final List<Annotation> members = option.members();
            final List<String> order = this.world.form(form).order();
            if (members.size() > order.size()) {
                throw new IllegalStateException(
                    String.format(
                        "The type %s gives %d types to %s, which has %d voids",
                        option, members.size(), form, order.size()
                    )
                );
            }
            for (int idx = 0; idx < members.size(); ++idx) {
                rows.put(order.get(idx), this.made(members.get(idx)));
            }
        }
        return this.copy(
            new Value(form, born, bits | this.bits(form, List.copyOf(rows.keySet()))), rows
        );
    }

    private int[] made(final Annotation member) {
        final Ints out = new Ints();
        for (final Annotation inner : member.options()) {
            final int copy = this.java(inner);
            this.state.remember(copy);
            out.add(copy);
        }
        return out.array();
    }

    private static boolean rooted(final int... rho) {
        boolean root = rho.length > 0;
        for (final int val : rho) {
            if (val != Values.ROOT) {
                root = false;
                break;
            }
        }
        return root;
    }
}
