/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
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
     * The lookups that are still running.
     */
    private final Set<Map.Entry<?, ?>> running;

    /**
     * For each lookup still running, the other running lookups it was led
     * back to, the innermost lookup on top.
     */
    private final Deque<Set<Map.Entry<?, ?>>> led;

    /**
     * Ctor.
     *
     * @param open The voids whose fillers are being looked at right now
     */
    Lookups(final Guesses open) {
        this.guesses = open;
        this.done = new HashMap<>(0);
        this.running = new HashSet<>(0);
        this.led = new ArrayDeque<>(0);
    }

    /**
     * What the lookup finds, done only the first time it is asked for.
     *
     * <p>While the lookup is in progress, asking for it again finds the copy
     * that stands for the lookup itself, as {@link Copy#looped()} says, which
     * cuts a cycle. A side that is this copy adds nothing to what the lookup
     * finds, while a name read off it, or a call on it, finds nothing
     * (#9983).</p>
     *
     * <p>What a lookup finds while it is led back to another lookup that is
     * still running is only true as long as that other lookup runs, so it is
     * not kept: when asked for again, the lookup is done again.</p>
     *
     * @param key The lookup
     * @param fresh How to do it
     * @return What it found
     */
    Optional<Copy> once(final Map.Entry<?, ?> key, final Supplier<Optional<Copy>> fresh) {
        final Optional<Copy> found;
        if (this.running.contains(key)) {
            this.led.peek().add(key);
            found = Optional.of(Copy.looped());
        } else if (this.done.containsKey(key)) {
            this.guesses.replay(this.done.get(key).getValue());
            found = this.done.get(key).getKey();
        } else {
            this.running.add(key);
            this.led.push(new HashSet<>(0));
            final Map.Entry<Optional<Copy>, Set<String>> made = this.guesses.tracked(fresh);
            final Set<Map.Entry<?, ?>> back = this.led.pop();
            this.running.remove(key);
            back.remove(key);
            if (back.isEmpty()) {
                this.done.put(key, made);
            } else if (!this.led.isEmpty()) {
                this.led.peek().addAll(back);
            }
            found = made.getKey();
        }
        return found;
    }
}
