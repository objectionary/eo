/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Statistics of a program that nobody is watching.
 *
 * <p>An object built outside a running program — in a test, or as a bottom
 * that belongs to no graph — reports its births and dispatches here, where
 * they are forgotten at once and cost nothing.</p>
 *
 * @since 0.62
 */
public final class Silent implements Statistics {

    @Override
    public void allocate() {
        // nothing to count
    }

    @Override
    public void dispatch() {
        // nothing to count
    }

    @Override
    public long allocations() {
        return 0L;
    }

    @Override
    public long dispatches() {
        return 0L;
    }
}
