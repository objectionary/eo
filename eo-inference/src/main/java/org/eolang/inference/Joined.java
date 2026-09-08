/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

/**
 * The one thing several objects all are.
 *
 * <p>Where a place in a program holds one of two objects and it is not known
 * which, there is still something to say about it, as long as the two of them
 * have a common ancestor: whichever one arrives, it is a copy of that. So each
 * object is walked back along what it delegates to, and the nearest step every
 * walk passes through is what they all are.</p>
 *
 * <p>Nearest is the point of it. Every object in EO ends at {@code Φ} and
 * saying so is saying nothing, so the walks are kept in the order they were
 * made and the first step they share is taken, which is the one furthest from
 * the root.</p>
 *
 * @since 0.71.0
 */
final class Joined {

    /**
     * The objects to join.
     */
    private final Collection<String> told;

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * Ctor.
     * @param objects The objects to join
     * @param provided What the types certainly have
     */
    Joined(final Collection<String> objects, final Provided provided) {
        this.told = objects;
        this.owned = provided;
    }

    /**
     * The nearest object all of them are copies of.
     * @return The locator, empty when they share nothing
     */
    String names() {
        final Iterator<String> rest = this.told.iterator();
        final List<String> shared = new ArrayList<>(0);
        if (rest.hasNext()) {
            shared.addAll(this.chain(rest.next()));
        }
        while (rest.hasNext()) {
            shared.retainAll(new HashSet<>(this.chain(rest.next())));
        }
        String found = "";
        if (!shared.isEmpty()) {
            found = shared.get(0);
        }
        return found;
    }

    private List<String> chain(final String type) {
        final List<String> found = new ArrayList<>(0);
        final Collection<String> seen = new HashSet<>(0);
        String walked = type;
        while (!walked.isEmpty() && seen.add(walked)) {
            found.add(walked);
            walked = this.owned.behind(walked);
        }
        return found;
    }
}
