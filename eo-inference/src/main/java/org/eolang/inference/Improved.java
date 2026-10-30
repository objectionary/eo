/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;

/**
 * Whether a freshly worked out answer says more about a dispatch than the one
 * already written down for it.
 *
 * <p>A pass answers a dispatch that has an answer already, because an answer
 * rooted at a void is true of every caller and concrete for none, and a later
 * pass knows more about the calls than an earlier one did. One that names a
 * formation wins over one rooted at a void however the two are related, since
 * the void was only ever standing in for it. Between two void-rooted answers
 * the shorter road wins, counted in the objects the locator walks through:
 * {@code Φ.bool.if.if.eq} and {@code Φ.bool.if.eq} are one call reached two
 * ways, and the second one is reached without a hop the program does not have.
 * A longer answer is refused, and so is another one of the same length,
 * because nothing in the tables prefers either road (#8855).</p>
 *
 * <p>That the count falls is what ends the passes. {@link Settled} goes round
 * again for as long as a pass writes anything down, so a rule that let one
 * dispatch be answered {@code a}, then {@code b}, then {@code a} again would
 * never let it stop. A locator walks through finitely many objects and never
 * through fewer than one, so a dispatch moves off its first void-rooted answer
 * only so many times.</p>
 *
 * @since 0.73.5
 */
final class Improved {

    /**
     * The locator of every void this pass may look into.
     */
    private final Collection<String> hollows;

    /**
     * The answer written down for the dispatch, empty when it has none.
     */
    private final String known;

    /**
     * Where the dispatch is written.
     */
    private final String made;

    /**
     * Ctor.
     *
     * @param voids The locator of every void this pass may look into, empty
     *  when it may look into none
     * @param recorded The answer written down for the dispatch, empty when it
     *  has none
     * @param site Where the dispatch is written
     */
    Improved(final Collection<String> voids, final String recorded, final String site) {
        this.hollows = voids;
        this.known = recorded;
        this.made = site;
    }

    /**
     * Whether this answer is worth writing down in place of the one there.
     *
     * @param kept The answer this pass worked out, empty when it worked none
     * @return True when the answer says more than the one on record
     */
    boolean on(final String kept) {
        final boolean found;
        if (kept.isEmpty() || kept.equals(this.made) || kept.equals(this.known)) {
            found = false;
        } else {
            found = this.known.isEmpty()
                || this.hollows.isEmpty() || !new Rooted(this.hollows).covers(kept)
                || kept.split("\\.").length < this.known.split("\\.").length;
        }
        return found;
    }
}
