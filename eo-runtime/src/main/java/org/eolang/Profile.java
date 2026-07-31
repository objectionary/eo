/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * The runtime statistics of a program: how many objects it created and
 * how many times it dispatched an attribute by name.
 *
 * @since 0.62
 */
public interface Profile {

    /**
     * The profile of the program running in this JVM, shared by every object
     * it creates, the same way {@link Phi#Φ} is shared.
     */
    Profile RUNNING = new Counters();

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
