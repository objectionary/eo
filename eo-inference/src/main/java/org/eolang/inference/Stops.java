/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Stops (design doc 4.4): what is decided when a pass grows nothing.
 *
 * <p>Decisions come in tiers, and a lower tier is decided only at a stop
 * where every tier above it decides nothing:</p>
 *
 * <ol>
 * <li>rules b, e and 3 decide everything that waits, at once;</li>
 * <li>change a: each void of an object an atom returned that was read
 * with nothing in its pool is decided "Java alone fills it";</li>
 * <li>stop w: the formations nobody calls get their study copies, or
 * their copies as written, and every real copy that has appeared since
 * the last such stop gets its study copies.</li>
 * </ol>
 *
 * <p>A decision is never taken back, and the run ends at a stop that
 * decides nothing.</p>
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
        final Set<Wish> fresh = this.state.rules().pending();
        final List<List<String>> java;
        if (fresh.isEmpty()) {
            java = this.state.pool().waiting();
        } else {
            java = Collections.emptyList();
        }
        final boolean quiet = fresh.isEmpty() && java.isEmpty();
        final Map<String, List<String>> nobody;
        if (quiet) {
            nobody = this.state.study().stays();
        } else {
            nobody = Collections.emptyMap();
        }
        this.state.pool().decide(java);
        this.state.rules().take(fresh);
        for (final Wish wish : fresh) {
            if (wish.rule() == 'e') {
                this.state.memo().stale(new Key('k', wish.value(), "", "", "", 0));
            } else {
                this.state.memo().stale(new Key('e', wish.ctx(), "", wish.place(), "", 0));
            }
        }
        this.state.study().pick(nobody);
        int kept = 0;
        if (quiet) {
            kept = this.state.study().studied();
        }
        return !fresh.isEmpty() || !java.isEmpty() || !nobody.isEmpty() || kept > 0;
    }

    /**
     * Whether anything waits for a stop.
     *
     * @return TRUE if something does
     */
    boolean pending() {
        return !this.state.rules().pending().isEmpty() || !this.state.pool().waiting().isEmpty();
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
