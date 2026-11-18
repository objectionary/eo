/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;

/**
 * The lookups {@link Origins} has done so far in one walk, each with what it
 * found and the open voids it ran into.
 *
 * <p>A lookup that is done again gives back what it found the first time,
 * and runs into the same open voids again, so that the lookup in progress
 * still knows it leans on what they are taken to hold. Without that, the
 * second of two fillers that read the same void would take an empty lookup
 * as a fact, and the void would be refused (#9981).</p>
 *
 * @since 0.76.0
 */
final class Lookups {

    /**
     * The voids whose fillers are being looked at right now.
     */
    private final Guesses guesses;

    /**
     * What each lookup found, and the open voids it ran into, by the lookup.
     */
    private final Map<Map.Entry<?, ?>, Map.Entry<Optional<Copy>, Set<String>>> done;

    /**
     * Ctor.
     *
     * @param open The voids whose fillers are being looked at right now
     */
    Lookups(final Guesses open) {
        this.guesses = open;
        this.done = new HashMap<>(0);
    }

    /**
     * What the lookup finds, done only the first time it is asked for.
     *
     * <p>While the lookup is in progress, asking for it again finds nothing,
     * which cuts a cycle.</p>
     *
     * @param key The lookup
     * @param fresh How to do it
     * @return What it found
     */
    Optional<Copy> once(final Map.Entry<?, ?> key, final Supplier<Optional<Copy>> fresh) {
        if (this.done.containsKey(key)) {
            this.guesses.replay(this.done.get(key).getValue());
        } else {
            this.done.put(key, Map.entry(Optional.empty(), Set.of()));
            this.done.replace(key, this.guesses.tracked(fresh));
        }
        return this.done.get(key).getKey();
    }
}
