/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;

/**
 * The one object a void holds, where the program puts one thing in it.
 *
 * <p>A void holds whatever a caller puts in it, and a program that puts one
 * thing in it everywhere has said what it holds: {@code Φ.string.printf.args}
 * is a {@code Φ.tuple} because every call site puts one there, and a build
 * reads the program whole, library and all, so there is no tomorrow for another
 * caller to arrive from.</p>
 *
 * <p>A void filled several ways is still a void. {@code Φ.bool.and.x} is filled
 * with a {@code Φ.true}, with a {@code Φ.false} and with five other things, and
 * naming any one of them would be picking a favourite among facts.</p>
 *
 * <p>So is a void whose one filling names nothing the provides table has a row
 * for — a datum, another void, an object the walk could not place. What is
 * worth having here is a locator a reader can go and look at, and whoever asks
 * what that object has needs a row to read the answer off.</p>
 *
 * @since 0.71.0
 */
final class Sole {

    /**
     * What the program was seen putting into the void, from {@link Fillings}.
     */
    private final Collection<Type> told;

    /**
     * The objects the provides table has a row for.
     */
    private final Collection<String> known;

    /**
     * Ctor.
     * @param witnesses What the program was seen putting into the void, from
     *  {@link Fillings}
     * @param objects The objects the provides table has a row for
     */
    Sole(final Collection<Type> witnesses, final Collection<String> objects) {
        this.told = witnesses;
        this.known = objects;
    }

    /**
     * The object this void holds.
     * @return The locator, empty where the void is filled several ways or its
     *  one filling names nothing a reader could go and look at
     */
    String names() {
        String found = "";
        if (this.told.size() == 1) {
            found = this.told.iterator().next().names();
        }
        if (!this.known.contains(found)) {
            found = "";
        }
        return found;
    }
}
