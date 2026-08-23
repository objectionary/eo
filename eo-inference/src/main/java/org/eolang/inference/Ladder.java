/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * How much of a program was understood.
 *
 * <p>The objects of the program counted by the rung they stand on, from the
 * shallowest up, and the two numbers that summarise them. A number without the
 * rungs beside it would be a number to game: writing an empty row for every
 * object would take {@link Ladder#described()} to a hundred and leave
 * {@link Ladder#percent()} where it was.</p>
 *
 * @since 0.69.0
 */
public final class Ladder {

    /**
     * How many objects stand on each rung.
     */
    private final Map<String, Integer> counts;

    /**
     * Ctor.
     * @param rungs How many objects stand on each rung, from the shallowest up
     */
    public Ladder(final Map<String, Integer> rungs) {
        this.counts = rungs;
    }

    /**
     * How many objects stand on each rung.
     * @return The rungs, from the shallowest up
     */
    public Map<String, Integer> rungs() {
        return Collections.unmodifiableMap(new LinkedHashMap<>(this.counts));
    }

    /**
     * How many objects the program has.
     * @return The objects
     */
    public int total() {
        return this.counts.values().stream().mapToInt(Integer::intValue).sum();
    }

    /**
     * How many of them we know anything at all about.
     * @return The share, out of a hundred
     */
    public double described() {
        final double result;
        if (this.counts.isEmpty()) {
            result = 0.0d;
        } else {
            result = this.share(this.total() - this.counts.values().iterator().next());
        }
        return result;
    }

    /**
     * How deeply the program is understood.
     *
     * <p>The mean rung, against the highest one there is, so that a program
     * every object of which was seen whole comes out at a hundred.</p>
     *
     * @return The depth, out of a hundred
     */
    public double percent() {
        final double result;
        if (this.counts.size() < 2) {
            result = 0.0d;
        } else {
            int climbed = 0;
            int rung = 0;
            for (final Integer count : this.counts.values()) {
                climbed = climbed + rung * count;
                rung = rung + 1;
            }
            result = this.share(climbed) / (rung - 1);
        }
        return result;
    }

    private double share(final int some) {
        final double found;
        if (this.total() == 0) {
            found = 0.0d;
        } else {
            found = 100.0d * some / this.total();
        }
        return found;
    }
}
