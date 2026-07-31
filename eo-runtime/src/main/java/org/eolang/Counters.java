/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.LongAdder;

/**
 * A profile that keeps its two numbers in a pair of adders.
 *
 * <p>The class is thread-safe.</p>
 *
 * @since 0.62
 */
public final class Counters implements Profile {

    /**
     * How many objects were born.
     */
    private final LongAdder born;

    /**
     * How many attributes were taken.
     */
    private final LongAdder taken;

    /**
     * Ctor.
     */
    public Counters() {
        this.born = new LongAdder();
        this.taken = new LongAdder();
    }

    @Override
    public void allocate() {
        this.born.increment();
    }

    @Override
    public void dispatch() {
        this.taken.increment();
    }

    @Override
    public long allocations() {
        return this.born.sum();
    }

    @Override
    public long dispatches() {
        return this.taken.sum();
    }
}
