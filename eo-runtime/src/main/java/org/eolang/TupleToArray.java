/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.function.Supplier;

/**
 * Convert {@code EOtuple} of arguments to Java array.
 *
 * @since 0.40.0
 */
public final class TupleToArray implements Supplier<Phi[]> {

    /**
     * Tuple of arguments.
     */
    private final Phi tuple;

    /**
     * Ctor.
     *
     * @param tup Tuple of arguments
     */
    public TupleToArray(final Phi tup) {
        this.tuple = tup;
    }

    @Override
    public Phi[] get() {
        final double raw = new Dataized(this.tuple.take("length")).asNumber().doubleValue();
        if (!Double.isFinite(raw) || raw < 0.0 || Math.rint(raw) != raw
            || raw > Integer.MAX_VALUE) {
            throw new ExFailure(
                "The tuple length must be a finite non-negative integer within int range, but it was %s",
                raw
            );
        }
        final int length = (int) raw;
        final Phi[] arguments = new Phi[length];
        Phi tup = this.tuple;
        for (int idx = length - 1; idx >= 0; --idx) {
            arguments[idx] = tup.take("head");
            tup = tup.take("tail");
        }
        return arguments;
    }
}
