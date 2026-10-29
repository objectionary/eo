/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The pool (design doc 4.5): what EO code puts into an empty void of an
 * object an atom returns, anywhere.
 *
 * <p>One row per formation and void, which grows inside the run, and a
 * growing pool is a change. This is the one place where values of
 * different calls meet on purpose. A void read with nothing in its pool
 * waits: at a stop where nothing else is decided, change a decides it is
 * one Java alone fills, and from then on it reads "open void" next to
 * whatever EO code puts into it later.</p>
 *
 * @since 0.76.0
 */
final class Pool {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * The world the walk goes over.
     */
    private final World world;

    /**
     * What EO code puts into each void, by formation and void.
     */
    private final Map<List<String>, Ints> rows;

    /**
     * The voids read with nothing in their pool.
     */
    private final Set<List<String>> nobody;

    /**
     * The voids change a decided Java alone fills.
     */
    private final Set<List<String>> decided;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     * @param world The world the walk goes over
     */
    Pool(final State walk, final World world) {
        this.state = walk;
        this.world = world;
        this.rows = new HashMap<>(0);
        this.nobody = new HashSet<>(0);
        this.decided = new HashSet<>(0);
    }

    /**
     * Read an empty void of a copy Java made, from its pool.
     *
     * @param copy The copy
     * @param name The void
     * @param site The site of the read, under the tag of the reader
     * @return What it holds, the decided open void too, nothing while it
     *  waits
     */
    Ints read(final int copy, final String name, final Site site) {
        final String form = this.world.value(copy).form();
        final List<String> pair = List.of(form, name);
        this.state.depend(new Key('p', 0, name, form, "", 0));
        final int[] got = this.rows.getOrDefault(pair, new Ints()).array();
        final boolean told = this.decided.contains(pair);
        final Ints out = new Ints();
        if (got.length == 0 && !told) {
            this.nobody.add(pair);
        } else {
            if (told) {
                out.add(Values.JGAP);
            }
            for (final int val : got) {
                final int stamped = this.state.stamp(val, copy, site);
                this.state.remember(stamped);
                out.add(stamped);
            }
        }
        return out;
    }

    /**
     * Put what a call hands to a void of what Java makes into its pool.
     *
     * @param form The formation
     * @param name The void
     * @param values The values
     */
    void fill(final String form, final String name, final int... values) {
        final Ints pool = this.rows.computeIfAbsent(List.of(form, name), key -> new Ints());
        boolean grew = false;
        for (final int val : values) {
            if (pool.add(val)) {
                grew = true;
            }
        }
        if (grew) {
            this.state.change();
            this.state.spoil(new Key('p', 0, name, form, "", 0));
        }
    }

    /**
     * Change a: the voids of objects atoms return that were read with
     * nothing in their pool, and are not decided yet.
     *
     * @return The voids, as formation and void, in order
     */
    List<List<String>> waiting() {
        final List<List<String>> out = new ArrayList<>(0);
        for (final List<String> pair : this.world.javaVoids()) {
            if (!this.decided.contains(pair) && this.nobody.contains(pair) && this.empty(pair)) {
                out.add(pair);
            }
        }
        return out;
    }

    /**
     * Decide that Java alone fills these voids.
     *
     * @param pairs The voids, as formation and void
     */
    void decide(final List<List<String>> pairs) {
        this.decided.addAll(pairs);
        for (final List<String> pair : pairs) {
            this.state.spoil(new Key('p', 0, pair.get(1), pair.get(0), "", 0));
        }
    }

    /**
     * Whether nobody fills a void: it was read with nothing in the pool and
     * never decided.
     *
     * @param form The formation
     * @param name The void
     * @return TRUE if nobody fills it
     */
    boolean unfilled(final String form, final String name) {
        final List<String> pair = List.of(form, name);
        return this.nobody.contains(pair) && this.empty(pair) && !this.decided.contains(pair);
    }

    private boolean empty(final List<String> pair) {
        return this.rows.getOrDefault(pair, new Ints()).isEmpty();
    }
}
