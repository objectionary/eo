/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * How much work a program did: how many objects it brought to life and how
 * many times it looked an attribute up by its name.
 *
 * <p>Both numbers are about {@link PhDefault}, since that is where an object
 * keeps its attributes and where a name is resolved. The decorators around
 * it delegate the lookup down to it, so a dispatch is counted once.</p>
 *
 * @since 0.73.3
 * @todo #3489:60min The statistics below belong to the JVM, not to a program,
 *  so a test cannot say what one object did and a run cannot switch the counting
 *  off. Give {@link PhDefault} a Statistics of its own, thread it through the
 *  constructors and through to-java.xsl, and let every object report to the
 *  program that made it.
 */
public interface Statistics {

    /**
     * Statistics of the program running in this JVM.
     */
    Statistics PROGRAM = new Counters();

    /**
     * Record the birth of one object.
     */
    void allocate();

    /**
     * Record one lookup of an attribute by its name.
     */
    void dispatch();

    /**
     * How many objects were born.
     * @return The number of births recorded
     */
    long allocations();

    /**
     * How many attributes were looked up.
     * @return The number of lookups recorded
     */
    long dispatches();
}
