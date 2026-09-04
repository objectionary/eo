/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Arrays;
import java.util.List;

/**
 * Which band an answer belongs to.
 *
 * <p>A rung is a number and a band is a colour, and the two are not the same
 * thing. The ladder has five rungs because a walk can end in five places, and
 * a page has four colours because a reader wants four different things done
 * about what they are looking at: a name they can go and read, a name that
 * waits on whoever calls them, a name that waits on Java, and nothing at
 * all.</p>
 *
 * <p>Two rungs share the green band, since a formation with voids still free
 * and a formation with none are both a place a reader can go and look at, and
 * one rung splits in two: a name rooted at a void is amber when the callers of
 * the program fill that void and violet when only an atom does. A void an atom
 * fills is not a gap in what we know — it is filled, in Java, where no caller
 * of the program can be looked at to find out with what, and telling a reader
 * to go and find the caller would send them looking for something that is not
 * there (#8352).</p>
 *
 * <p>The band is worked out here and nowhere else. A page that colours a word
 * and a tally that counts it are two readers of the same answer, and the one
 * way to be sure they cannot disagree is for neither of them to know how a
 * colour is arrived at.</p>
 *
 * @since 0.71.0
 */
final class Band {

    /**
     * The bands, from the least known to the most.
     */
    private static final List<String> NAMES = Arrays.asList(
        "blank", "rooted", "atom", "named"
    );

    /**
     * The answer.
     */
    private final Answer told;

    /**
     * Ctor.
     * @param answer The answer, which says where a walk ended and how far up
     */
    Band(final Answer answer) {
        this.told = answer;
    }

    /**
     * The name of the band.
     * @return The name, which a page puts on a span and a tally counts under
     */
    String name() {
        return Band.NAMES.get(this.rank());
    }

    /**
     * How much is known, on the bands rather than on the rungs.
     * @return The rank, from nothing at all up to a formation we can name
     */
    int rank() {
        final int found;
        if (this.told.rung() == 0) {
            found = 0;
        } else if (this.told.rung() > 1) {
            found = Band.NAMES.size() - 1;
        } else if (this.told.forged()) {
            found = 2;
        } else {
            found = 1;
        }
        return found;
    }
}
