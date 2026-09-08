/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;

/**
 * The voids a fact may be about.
 *
 * <p>A void is not the only object that facts about it are written against.
 * The program asks {@code x.next} for a {@code foo}, and that is a fact about
 * the same void one step further out: whatever fills {@code x} will have to
 * hand over a {@code next} that has a {@code foo}. So is anything said about a
 * void this one is handed into, since what fills this one arrives there
 * too.</p>
 *
 * <p>Which facts belong to a void is therefore a question about locators and
 * not about the facts, and every kind of fact asks it the same way:
 * {@link Demands} for a name taken off a void, {@link Applies} for a call made
 * on one. Which of the voids it is belongs here too, since a reader who has to
 * go and look the void up is asking the same question one answer further.</p>
 *
 * @since 0.72.0
 */
final class Rooted {

    /**
     * The voids: one of them, and every void it is handed into.
     */
    private final Collection<String> voids;

    /**
     * Ctor.
     * @param objects The voids a fact may be about: the void itself, and every
     *  void it is handed into
     */
    Rooted(final Collection<String> objects) {
        this.voids = objects;
    }

    /**
     * Whether a fact about this object is a fact about one of these voids.
     * @param object The locator of the object the fact is written against
     * @return True when it is one of the voids, or a name rooted at one
     */
    boolean covers(final String object) {
        return !this.names(object).isEmpty();
    }

    /**
     * The void this object is rooted at.
     * @param object The locator of the object the fact is written against
     * @return The locator of the nearest of these voids the name is taken off,
     *  empty when the name is rooted at none of them
     */
    String names(final String object) {
        String found = "";
        String walked = object;
        while (!walked.isEmpty()) {
            if (this.voids.contains(walked)) {
                found = walked;
                break;
            }
            if (!walked.contains(".")) {
                break;
            }
            walked = walked.substring(0, walked.lastIndexOf('.'));
        }
        return found;
    }
}
