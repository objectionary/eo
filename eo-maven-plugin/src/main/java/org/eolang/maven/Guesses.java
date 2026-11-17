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
 * The voids whose fillers {@link Origins} is looking at right now, with what
 * each of them is taken to hold meanwhile.
 *
 * <p>A void may be filled with what is read off itself, when a helper passes
 * its own void back to itself. While its fillers are looked at, the void is
 * open here: first it is taken to hold nothing, and then what the round
 * before found. A lookup that runs into an open void gets that guess, and
 * the void is noted as touched by every lookup in progress, so that a
 * lookup can tell whether what it found leans on a guess.</p>
 *
 * @since 0.76.0
 */
final class Guesses {

    /**
     * What each open void is taken to hold, by its place: nothing yet, or
     * what the round before found.
     */
    private final Map<String, Optional<Copy>> held;

    /**
     * The open voids each lookup in progress ran into, the innermost lookup
     * on top.
     */
    private final Deque<Set<String>> touched;

    /**
     * Ctor.
     */
    Guesses() {
        this.held = new HashMap<>(0);
        this.touched = new ArrayDeque<>(0);
    }

    /**
     * Whether the void is open.
     *
     * @param place The place of the void
     * @return TRUE if its fillers are being looked at
     */
    boolean open(final String place) {
        return this.held.containsKey(place);
    }

    /**
     * What an open void is taken to hold, noting that the lookup in progress
     * ran into it.
     *
     * @param place The place of the void
     * @return The guess, or nothing in the first round
     */
    Optional<Copy> hit(final String place) {
        if (!this.touched.isEmpty()) {
            this.touched.peek().add(place);
        }
        return this.held.get(place);
    }

    /**
     * Take the void to hold something, and open it, if it is not open yet.
     *
     * @param place The place of the void
     * @param guess What it is taken to hold, or nothing
     */
    void guess(final String place, final Optional<Copy> guess) {
        this.held.put(place, guess);
    }

    /**
     * Close the void, when its fillers are all looked at.
     *
     * @param place The place of the void
     */
    void close(final String place) {
        this.held.remove(place);
    }

    /**
     * Run a lookup and tell which open voids it ran into.
     *
     * @param lookup The lookup
     * @return What it found, and the voids it ran into
     */
    Map.Entry<Optional<Copy>, Set<String>> tracked(final Supplier<Optional<Copy>> lookup) {
        this.touched.push(new HashSet<>(0));
        final Optional<Copy> found = lookup.get();
        final Set<String> ran = this.touched.pop();
        if (!this.touched.isEmpty()) {
            this.touched.peek().addAll(ran);
        }
        return Map.entry(found, ran);
    }

    /**
     * Whether some of the voids is still open with no guess yet, so that a
     * lookup that ran into it and found nothing may find something in the
     * next round.
     *
     * @param places The places of the voids
     * @return TRUE if one of them is
     */
    boolean blind(final Set<String> places) {
        return places.stream().anyMatch(
            place -> this.held.containsKey(place) && this.held.get(place).isEmpty()
        );
    }

    /**
     * Whether some of the voids is still open, so that what a lookup that
     * ran into it found leans on a guess and may not be remembered.
     *
     * @param places The places of the voids
     * @return TRUE if one of them is
     */
    boolean leaning(final Set<String> places) {
        return places.stream().anyMatch(this.held::containsKey);
    }
}
