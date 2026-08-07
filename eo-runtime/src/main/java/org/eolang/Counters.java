/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.LongAdder;

/**
 * Statistics kept in a pair of adders.
 *
 * <p>Either counter may be incremented by many threads at once. The two are
 * read one after another, so a pair taken while the program still runs does
 * not belong to one instant.</p>
 *
 * @since 0.73.3
 */
public final class Counters implements Statistics {

    /**
     * How many objects were born.
     */
    private final LongAdder births;

    /**
     * How many attributes were looked up.
     */
    private final LongAdder lookups;

    /**
     * Ctor.
     */
    public Counters() {
        this.births = new LongAdder();
        this.lookups = new LongAdder();
    }

    @Override
    public void allocate() {
        this.births.increment();
    }

    @Override
    public void dispatch() {
        this.lookups.increment();
    }

    @Override
    public long allocations() {
        return this.births.sum();
    }

    @Override
    public long dispatches() {
        return this.lookups.sum();
    }
}
