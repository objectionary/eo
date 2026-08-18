/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * The runtime statistics of a program: how many objects it brought to life
 * and how many times it dispatched an attribute by name.
 *
 * <p>Both numbers are about {@link PhDefault}, since that is where objects
 * hold their attributes. The decorators around it — {@link PhSafe},
 * {@link PhOnce}, {@link PhNest}, {@link PhCoverage}, {@link PhLogged} —
 * are not counted as objects, and the dispatches they delegate are counted
 * once, at the object that resolves the name.</p>
 *
 * @since 0.62
 */
public interface Statistics {

    /**
     * Record the birth of one object.
     */
    void allocate();

    /**
     * Record one dynamic dispatch, a lookup of an attribute by its name.
     */
    void dispatch();

    /**
     * How many objects were born.
     * @return The number of allocations
     */
    long allocations();

    /**
     * How many dispatches were made.
     * @return The number of dispatches
     */
    long dispatches();
}
