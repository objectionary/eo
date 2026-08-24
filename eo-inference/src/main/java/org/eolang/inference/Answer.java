/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

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
     * Ctor.
     * @param where The object this one settled on, which is the object itself
     *  when the walk went nowhere
     * @param rung The rung it stands on, from nothing at all up to nothing
     *  left to find out
     */
    Answer(final String where, final int rung) {
        this.settled = where;
        this.climbed = rung;
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
}
