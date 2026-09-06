/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.LinkedHashSet;

/**
 * The one thing a void holds, where the program fills it several ways.
 *
 * <p>{@link Sole} refuses a void filled with a {@code Φ.true} here and a
 * {@code Φ.false} there, since naming either would be picking a favourite
 * among facts. Naming what the two of them are is not picking one: both hand
 * their answers to a {@code Φ.bool}, so every caller of that void puts a
 * {@code Φ.bool} there, whichever one it is. That is worth having, because a
 * dispatch rooted at the void can go on past it.</p>
 *
 * <p>It is worth having for the dispatches and not for the void itself, whose
 * band stays where a name rooted at a void belongs, with the union of what was
 * seen in it beside it. A reader asking about the void wants the callers, not
 * their common ancestor.</p>
 *
 * @since 0.71.0
 */
final class Shared {

    /**
     * What the program was seen putting into the void, from {@link Fillings}.
     */
    private final Collection<Type> told;

    /**
     * The objects the provides table has a row for.
     */
    private final Collection<String> known;

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * Ctor.
     * @param witnesses What the program was seen putting into the void, from
     *  {@link Fillings}
     * @param objects The objects the provides table has a row for
     * @param provided What the types certainly have
     */
    Shared(
        final Collection<Type> witnesses,
        final Collection<String> objects,
        final Provided provided
    ) {
        this.told = witnesses;
        this.known = objects;
        this.owned = provided;
    }

    /**
     * The object every filling of this void is.
     * @return The locator, empty where the fillings share nothing a reader
     *  could go and look at
     */
    String names() {
        final Collection<String> handed = new LinkedHashSet<>(0);
        for (final Type one : this.told) {
            handed.add(one.names());
        }
        String found = new Joined(handed, this.owned).names();
        if (!this.known.contains(found)) {
            found = "";
        }
        return found;
    }
}
