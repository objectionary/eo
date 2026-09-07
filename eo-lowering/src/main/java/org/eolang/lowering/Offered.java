/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

/**
 * The methods the universe answers for a forma.
 *
 * <p>A symbolic carrier of a forma dispatches inside the {@link Universe}
 * exactly the methods the {@link Op} table binds to that forma, since
 * the universe is written from the table: those methods phino resolves,
 * fires or parks, and the records tell. A number, a string and a bool
 * decorate their own bytes, the way the carriers of the runtime do, so
 * they answer the bytes operations as well. Any other method dispatched
 * on a settled receiver has nowhere to go inside the universe, so a
 * {@link Site} renders such a dispatch as a marker phino parks on, and
 * the reduction turns the marker into a step that dispatches back into
 * EO, by {@link Dispatched}.</p>
 *
 * @since 0.76.0
 */
public final class Offered {

    /**
     * The forma of the receiver.
     */
    private final String forma;

    /**
     * Ctor.
     * @param carrier The forma of the receiver, such as {@code number}
     */
    public Offered(final String carrier) {
        this.forma = carrier;
    }

    /**
     * Whether the universe answers the method for the forma.
     * @param method The method, such as {@code plus}
     * @return True if phino resolves the method on a carrier of the forma
     */
    public boolean has(final String method) {
        boolean out = false;
        for (final String[] row : Op.table()) {
            if (row[1].equals(method) && this.reaches(row[2])) {
                out = true;
                break;
            }
        }
        return out;
    }

    private boolean reaches(final String carrier) {
        return carrier.equals(this.forma)
            || "bytes".equals(carrier) && Offered.decorated(this.forma);
    }

    private static boolean decorated(final String forma) {
        return "number".equals(forma) || "string".equals(forma) || "bool".equals(forma);
    }
}
