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
 * into a {@code ρ}. An object an atom returns is one copy per formation,
 * born {@code java}.</p>
 *
 * @since 0.76.0
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
            if (!Values.JBORN.equals(name.birth()) && (name.bound() & ~Formation.RHO) == 0
                && Copies.rooted(rows.getOrDefault("ρ", new int[0]))) {
                made = this.blank.get(form);
            } else {
                made = this.canon.get(form);
            }
        } else {
            made = this.world.intern(
                new Value(
                    'O', form, name.birth(), name.bound() & this.mask(form), name.tag(), -1, 0
                )
            );
            for (final Map.Entry<String, int[]> row : rows.entrySet()) {
                this.state.gather(made, row.getKey(), row.getValue());
            }
        }
        return made;
    }

    /**
     * A copy with nothing in its voids, and its {@code ρ} only if given:
     * none, Java's own, or a value.
     *
     * @param form The formation
     * @param born Where it is born, with its tag
     * @param rho The {@code ρ}: {@link #NO_RHO}, {@link Values#JAVA} or a value
     * @return The copy
     */
    int fresh(final String form, final Site born, final int rho) {
        final int made;
        if (rho == Copies.NO_RHO) {
            made = this.copy(new Value(form, born, 0L), Map.of());
        } else if (rho == Values.JAVA) {
            made = this.java(form);
        } else {
            made = this.copy(new Value(form, born, Formation.RHO), Map.of("ρ", new int[] {rho}));
        }
        return made;
    }

    /**
     * The copy of a formation Java makes, with Java's own {@code ρ}.
     *
     * @param form The formation
     * @return The copy
     */
    int java(final String form) {
        return this.copy(new Value(form, new Site(Values.JBORN, ""), Formation.RHO), Map.of());
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
     * The bits of every void of a formation, {@code ρ} too.
     *
     * @param form The formation
     * @return The bits
     */
    long mask(final String form) {
        return (1L << this.world.form(form).order().size()) - 1L | Formation.RHO;
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
                this.world.intern(
                    new Value('O', kind, "canon", 1L | Formation.RHO, "", -1, 0)
                )
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
            this.state.seed(shared, "ρ", Values.ROOT);
        }
        for (final String kind : kinds) {
            final int empty = this.world.intern(
                new Value('O', kind, "blank", Formation.RHO, "", -1, 0)
            );
            this.blank.put(kind, empty);
            this.state.seed(empty, "ρ", Values.ROOT);
        }
        for (final String kind : kinds) {
            this.joins.add(this.blank.get(kind));
        }
        for (final String kind : kinds) {
            this.joins.add(this.canon.get(kind));
        }
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
