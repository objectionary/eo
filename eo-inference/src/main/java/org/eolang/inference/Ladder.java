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
 * shallowest up, and the numbers that summarise them. Three of those are
 * shares of the whole and answer the question anyone actually asks: of every
 * object in the program, how many do we know the formation of, how many can
 * only be described by pointing at somebody else's void, and how many are we
 * silent about. They are {@link Ladder#named()}, {@link Ladder#rooted()} and
 * {@link Ladder#blank()}, and the three add up to a hundred.</p>
 *
 * <p>{@link Ladder#percent()} is a fourth and a different kind of number: the
 * mean rung, which is an average of an ordinal scale and so means nothing on
 * its own. It is kept because it moves when a rule gets sharper without moving
 * an object across a band, and it is kept behind the three, never in front of
 * them.</p>
 *
 * <p>None of them may be read without the rungs beside it, since a share is a
 * number to game. Writing a row saying nothing about every object would leave
 * every one of these where it was, which is the point of counting the rungs
 * apart in the first place.</p>
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
     *
     * @param rungs How many objects stand on each rung, from the shallowest up
     */
    public Ladder(final Map<String, Integer> rungs) {
        this.counts = new LinkedHashMap<>(rungs);
    }

    /**
     * How many objects stand on each rung.
     *
     * @return The rungs, from the shallowest up
     */
    public Map<String, Integer> rungs() {
        return Collections.unmodifiableMap(this.counts);
    }

    /**
     * How many objects the program has.
     *
     * @return The objects
     */
    public int total() {
        return this.counts.values().stream().mapToInt(Integer::intValue).sum();
    }

    /**
     * How many of them we know the formation of.
     *
     * <p>This is coverage. An object stands here when the answer to what it
     * was copied from is a formation of the program, whatever is still free
     * inside it, and a datum and a termination stand here too, being answers
     * that leave nothing to ask.</p>
     *
     * @return The share, out of a hundred
     */
    public double named() {
        return this.share(this.total() - this.upto(2));
    }

    /**
     * How many of them we can only describe through somebody else's void.
     *
     * <p>{@code Φ.inc.x.next} is the {@code next} of whatever fills {@code x},
     * which is true of every caller and names no formation, so it is not
     * coverage. It is not nothing either, and counting it as either would be a
     * lie in one direction or the other.</p>
     *
     * @return The share, out of a hundred
     */
    public double rooted() {
        return this.share(this.upto(2) - this.upto(1));
    }

    /**
     * How many of them we say nothing about.
     *
     * @return The share, out of a hundred
     */
    public double blank() {
        return this.share(this.upto(1));
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

    private int upto(final int rungs) {
        int found = 0;
        int rung = 0;
        for (final Integer count : this.counts.values()) {
            if (rung < rungs) {
                found = found + count;
            }
            rung = rung + 1;
        }
        return found;
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
