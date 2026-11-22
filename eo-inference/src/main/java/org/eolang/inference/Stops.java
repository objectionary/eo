/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Stops (design doc 4.4): what is decided when a pass grows nothing.
 *
 * <p>Decisions come in tiers, and a lower tier is decided only at a stop
 * where the tier above it decides nothing:</p>
 *
 * <ol>
 * <li>rules b, e and 3 decide everything that waits, at once;</li>
 * <li>stop w: the formations nobody calls whose copies as written do not
 * count get their study copies, or their copies as written, a round at a
 * time, and every real copy that has appeared since the last such stop
 * gets its study copies.</li>
 * </ol>
 *
 * <p>A void of an object an atom returned waits for no stop: the type of
 * the atom says what it holds, or it holds the unknown only Java fills, from
 * its first read (4.5). A decision is never taken back, and the run ends at
 * a stop that decides nothing.</p>
 *
 * @since 0.76.0
 */
final class Stops {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * How many stops there were.
     */
    private final AtomicInteger count;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    Stops(final State walk) {
        this.state = walk;
        this.count = new AtomicInteger();
    }

    /**
     * Decide at a stop.
     *
     * @return TRUE if anything was decided
     */
    boolean decide() {
        this.count.incrementAndGet();
        final Set<Wish> fresh = this.state.wishes();
        this.state.take(fresh);
        for (final Wish wish : fresh) {
            if (wish.rule() == 'e') {
                this.state.stale(new Key('k', wish.value(), "", "", "", 0));
            } else {
                this.state.stale(new Key('e', wish.ctx(), "", wish.place(), "", 0));
            }
        }
        boolean picked = false;
        int kept = 0;
        if (fresh.isEmpty()) {
            picked = this.state.pick();
            kept = this.state.studied();
        }
        return !fresh.isEmpty() || picked || kept > 0;
    }

    /**
     * Whether anything waits for a stop.
     *
     * @return TRUE if something does
     */
    boolean pending() {
        return !this.state.wishes().isEmpty();
    }

    /**
     * How many stops there were.
     *
     * @return The count
     */
    int size() {
        return this.count.get();
    }
}
