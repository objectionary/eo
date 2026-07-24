/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * The outcome of taking an attribute from an object.
 *
 * <p>It pairs the object that was taken with a flag telling whether that
 * object is pure, that is, whether the φ-expression that produced it always
 * yields the same value. Caching decorators such as {@code PhSticky} memoize
 * the object only when it is pure; an impure result (file access, the clock,
 * shared memory) is never stored.</p>
 *
 * @since 0.60
 */
public final class Taken {

    /**
     * The object taken.
     */
    private final Phi object;

    /**
     * Whether the object may be memoized.
     */
    private final boolean clean;

    /**
     * Ctor.
     * @param object The object taken
     * @param clean Whether the object may be memoized
     */
    public Taken(final Phi object, final boolean clean) {
        this.object = object;
        this.clean = clean;
    }

    /**
     * The object that was taken.
     * @return The object
     */
    public Phi phi() {
        return this.object;
    }

    /**
     * Whether the taken object is pure and may be memoized.
     * @return True if the object may be cached
     */
    public boolean pure() {
        return this.clean;
    }
}
