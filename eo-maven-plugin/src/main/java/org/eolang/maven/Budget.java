/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

/**
 * How much wall clock a step is allowed to burn.
 *
 * <p>The counting starts the moment the budget is made, and the budget
 * is spent once that many seconds have passed. A budget of zero seconds
 * is never spent, which is how a step is told to run for as long as it
 * needs.</p>
 *
 * @since 0.76.0
 */
final class Budget {

    /**
     * How many seconds are allowed, where zero means no limit.
     */
    private final long seconds;

    /**
     * When the counting started, in milliseconds.
     */
    private final long start;

    /**
     * Ctor.
     *
     * @param total How many seconds are allowed, where zero means no limit
     */
    Budget(final long total) {
        this(total, System.currentTimeMillis());
    }

    /**
     * Ctor.
     *
     * @param total How many seconds are allowed, where zero means no limit
     * @param since When the counting started, in milliseconds
     */
    Budget(final long total, final long since) {
        this.seconds = total;
        this.start = since;
    }

    /**
     * Is there nothing left to spend?
     *
     * @return TRUE if the seconds are over
     */
    boolean spent() {
        return this.seconds > 0L
            && System.currentTimeMillis() - this.start >= this.seconds * 1000L;
    }
}
