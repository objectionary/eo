/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven.dealpha;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * What {@link Fillers} saw of the voids around it while it worked out what
 * one void holds.
 *
 * <p>A void is open while its fillers are being looked at, and meanwhile it
 * is taken to hold a guess, as {@link Guesses} says. The work on a void
 * depends on the open voids only in two ways: what it found in the open
 * voids it ran into, which is their guesses, and which voids it found not
 * open, so that it opened them and worked them out itself. A situation keeps
 * both: the open voids it leaned on, each with its guess, and the voids it
 * opened.</p>
 *
 * @since 0.76.0
 */
final class Situation {

    /**
     * The open voids the work leaned on, each with what it was taken to hold
     * then, by the place of the void.
     */
    private final Map<String, Optional<Copy>> leaned;

    /**
     * The places of the voids the work opened itself.
     */
    private final Set<String> opened;

    /**
     * Ctor.
     *
     * @param guesses The open voids the work leaned on, with their guesses
     * @param own The places of the voids the work opened itself
     */
    Situation(final Map<String, Optional<Copy>> guesses, final Set<String> own) {
        this.leaned = guesses;
        this.opened = own;
    }

    /**
     * The open voids the work leaned on, each with its guess.
     *
     * @return The guesses, by the place of the void
     */
    Map<String, Optional<Copy>> leaned() {
        return this.leaned;
    }

    /**
     * The voids the work opened itself.
     *
     * @return Their places
     */
    Set<String> opened() {
        return this.opened;
    }
}
