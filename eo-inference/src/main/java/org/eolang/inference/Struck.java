/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Map;

/**
 * A run of the passes that struck the arms reading a void nothing fills.
 *
 * <p>Three things come out of one such run and all three are wanted. The
 * dispatches that struck the arms, since whoever asks what the program turns
 * out to be asks them and nobody else. The pairs they settled, which is what
 * the tables are written from. And the voids that run left unfilled, which
 * {@link Emptied} holds against the ones it went in with, to see whether the
 * passes have to go round again.</p>
 *
 * <p>They are kept together because the run that works one of them out works
 * out all three, and asking for them apart made the run again: {@link Resolved}
 * settled the pairs of the very run {@link Emptied} had just ended on, which on
 * {@code eo-runtime} was 85 of the 467 passes a build makes (#9648).</p>
 *
 * @since 0.74.0
 */
final class Struck {

    /**
     * The dispatches, striking every arm that reads a void nothing fills.
     */
    private final Dispatched made;

    /**
     * The pairs the run settled.
     */
    private final Map<String, String> settled;

    /**
     * The voids the run left unfilled.
     */
    private final Collection<String> hollows;

    /**
     * Ctor.
     *
     * @param dispatched The dispatches, striking every arm that reads a void
     *  nothing fills
     * @param pairs The pairs the run settled
     * @param voids The voids the run left unfilled
     */
    Struck(
        final Dispatched dispatched,
        final Map<String, String> pairs,
        final Collection<String> voids
    ) {
        this.made = dispatched;
        this.settled = pairs;
        this.hollows = voids;
    }

    /**
     * The dispatches that struck the arms.
     *
     * @return The dispatches
     */
    Dispatched dispatches() {
        return this.made;
    }

    /**
     * The pairs the run settled.
     *
     * @return The pairs, each name against the one it is a copy of
     */
    Map<String, String> pairs() {
        return this.settled;
    }

    /**
     * The voids the run left unfilled.
     *
     * @return The locators of the voids
     */
    Collection<String> empty() {
        return this.hollows;
    }
}
