/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * The rules that wait for a stop (design doc 4.3): b, e and 3.
 *
 * <p>A call whose argument is only an open void, an object whose {@code @}
 * gives nothing typed, and an argument whose every value fails give nothing
 * until a stop decides them; a decision is never taken back. What waits
 * belongs to the answer that found it waiting, and leaves when that answer
 * is worked out again or loses its answer, so what waits after a pass is
 * what the answers of that pass found.</p>
 *
 * @since 0.76.0
 */
final class Rules {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * What waits, by the answer that found it waiting.
     */
    private final Map<Integer, Set<Wish>> wishes;

    /**
     * What the stops decided.
     */
    private final Set<Wish> taken;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Rules(final State walk) {
        this.state = walk;
        this.wishes = new HashMap<>(0);
        this.taken = new HashSet<>(0);
    }

    /**
     * Note that something waits for a stop.
     *
     * @param wish What waits
     */
    void wish(final Wish wish) {
        final int owner;
        if (this.state.memo().busy()) {
            owner = this.state.memo().owner();
        } else {
            owner = -1;
        }
        this.wishes.computeIfAbsent(owner, key -> new HashSet<>(1)).add(wish);
    }

    /**
     * Whether a stop decided something.
     *
     * @param wish What waited
     * @return TRUE if a stop decided it
     */
    boolean taken(final Wish wish) {
        return this.taken.contains(wish);
    }

    /**
     * What waits now and no stop decided yet.
     *
     * @return What waits
     */
    Set<Wish> pending() {
        final Set<Wish> out = new LinkedHashSet<>(0);
        for (final Set<Wish> got : this.wishes.values()) {
            for (final Wish wish : got) {
                if (!this.taken.contains(wish)) {
                    out.add(wish);
                }
            }
        }
        return out;
    }

    /**
     * Decide what waits, for good.
     *
     * @param decided What waits
     */
    void take(final Set<Wish> decided) {
        this.taken.addAll(decided);
    }

    /**
     * Forget what an answer found waiting, since it lost its answer.
     *
     * @param owner The answer
     */
    void forget(final int owner) {
        this.wishes.remove(owner);
    }

    /**
     * Forget everything that waits, before a full pass.
     */
    void clear() {
        this.wishes.clear();
    }
}
