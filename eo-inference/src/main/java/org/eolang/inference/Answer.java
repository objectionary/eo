/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;

/**
 * What one object of the program turns out to be.
 *
 * <p>Two things are worth knowing about an object and they are worked out
 * together, from the same walk of the same tables: which object it settled on,
 * and how much that settling is worth. The first is the answer a reader wants
 * — {@code Φ.number} is a place they can go and look at — and the second is
 * the rung it stands on, which is all a count of the whole program needs.</p>
 *
 * <p>They are kept in one object so they cannot drift apart. A page that
 * colours an object green while the printed number counts it among the ones
 * we know nothing about is worse than either alone, and the only way to be
 * sure that cannot happen is for both to read the same answer.</p>
 *
 * <p>An object that settled on nothing better than somebody else's void
 * carries what the program was seen putting into that void as well. A void
 * filled one way and no other is that one thing and settles there instead, so
 * what is carried here is what the walk could not settle on — several types at
 * once, or a single one naming nothing a reader could go and look at. A reader
 * told that their object is whatever {@code Φ.bool.and.x} turns out to be has
 * nowhere to go next, and a reader told that {@code Φ.true} and
 * {@code Φ.false} have both been put there has. The rung is untouched by
 * it.</p>
 *
 * <p>Such an object also says whether the void it is rooted at is one that
 * only an atom fills. {@code Φ.posix.return.code} is filled in Java, by the
 * syscall that hands the object back, and no caller of the program can be
 * looked at to find out what goes in there — which is a different thing to
 * say than that the callers disagree, and asks a different thing of whoever
 * reads it (#8352). The rung is untouched by this as well: it is the same
 * name rooted at the same void, and only the reason it stayed there
 * differs.</p>
 *
 * @since 0.70.0
 */
final class Answer {

    /**
     * The object this one settled on.
     */
    private final String settled;

    /**
     * The rung it stands on.
     */
    private final int climbed;

    /**
     * What the program was seen putting into the void it is rooted at.
     */
    private final Collection<Type> witnesses;

    /**
     * Whether that void is one only an atom fills.
     */
    private final boolean hammered;

    /**
     * Ctor.
     * @param where The object this one settled on, which is the object itself
     *  when the walk went nowhere
     * @param rung The rung it stands on, from nothing at all up to nothing
     *  left to find out
     */
    Answer(final String where, final int rung) {
        this(where, rung, Collections.emptyList());
    }

    /**
     * Ctor.
     * @param where The object this one settled on, which is the object itself
     *  when the walk went nowhere
     * @param rung The rung it stands on, from nothing at all up to nothing
     *  left to find out
     * @param seen What the program was seen putting into the void it is
     *  rooted at, empty when it is rooted at none or nobody fills it
     */
    Answer(final String where, final int rung, final Collection<Type> seen) {
        this(where, rung, seen, false);
    }

    /**
     * Ctor.
     * @param where The object this one settled on, which is the object itself
     *  when the walk went nowhere
     * @param rung The rung it stands on, from nothing at all up to nothing
     *  left to find out
     * @param seen What the program was seen putting into the void it is
     *  rooted at, empty when it is rooted at none or nobody fills it
     * @param atom Whether that void is one only an atom fills
     */
    Answer(final String where, final int rung, final Collection<Type> seen,
        final boolean atom) {
        this.settled = where;
        this.climbed = rung;
        this.witnesses = seen;
        this.hammered = atom;
    }

    /**
     * The object this one settled on.
     * @return The locator
     */
    String where() {
        return this.settled;
    }

    /**
     * The rung it stands on.
     * @return The rung, from nothing at all up to nothing left to find out
     */
    int rung() {
        return this.climbed;
    }

    /**
     * What the program was seen putting into the void it is rooted at.
     * @return The types, empty when nobody was seen filling it
     */
    Collection<Type> seen() {
        return this.witnesses;
    }

    /**
     * Whether the void it is rooted at is one only an atom fills.
     * @return TRUE when no caller of the program can be looked at
     */
    boolean forged() {
        return this.hammered;
    }
}
