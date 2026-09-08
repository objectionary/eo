/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.cactoos.Text;

/**
 * Elapsed nanoseconds printed as milliseconds, rounded up, so that
 * a span shorter than a millisecond reads as one instead of zero.
 *
 * @since 0.73.4
 */
final class Millis implements Text {

    /**
     * Nanoseconds elapsed.
     */
    private final long nanos;

    /**
     * Ctor.
     *
     * @param span Nanoseconds elapsed
     */
    Millis(final long span) {
        this.nanos = span;
    }

    @Override
    public String asString() {
        return String.valueOf((this.nanos + 999_999L) / 1_000_000L);
    }
}
