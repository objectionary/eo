/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;

/**
 * Where a name points, in a program.
 *
 * <p>Every name is split off its head before any clue looks, so a chain
 * starts from a bare {@code ξ} or a bare {@code Φ} and nothing else, and those
 * two are the only references there are. This is the object that turns either
 * into the locator it means, which is the first thing in the checker that has
 * to look beyond the object in front of it. A bare {@code Φ} is the global
 * object itself, so it points at {@code Φ}. A bare {@code ξ} is the object it
 * is written in, so it points at the nearest formation around it, which
 * {@link Nesting} finds by the text alone.</p>
 *
 * <p>Every name after the head is a dispatch and is no business of this
 * object: which attribute {@code ξ.x} turns out to be, and whether {@code ^}
 * names a receiver the formation declares, is answered from the tables once
 * the head is known. The checker is allowed to know less; it is not allowed
 * to guess, because a wrong link would make every later answer wrong with
 * it.</p>
 *
 * @since 0.68.0
 */
final class Scope {

    /**
     * The locators of the formations, the only objects a {@code ξ} can be.
     */
    private final Collection<String> formations;

    /**
     * Ctor.
     *
     * @param made The locators of the formations of the program
     */
    Scope(final Collection<String> made) {
        this.formations = made;
    }

    /**
     * The locator the given reference points at.
     *
     * @param reference The locator of the reference itself
     * @param base The base it carries, {@code ξ} or {@code Φ}
     * @return The locator
     */
    String target(final String reference, final String base) {
        final String found;
        if ("Φ".equals(base)) {
            found = base;
        } else if ("ξ".equals(base)) {
            found = new Nesting(this.formations).around(reference);
        } else {
            throw new IllegalStateException(
                String.format(
                    "The reference at %s with base '%s' points at nothing",
                    reference, base
                )
            );
        }
        return found;
    }
}
