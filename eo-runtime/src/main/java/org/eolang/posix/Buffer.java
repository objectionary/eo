/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.ExFailure;

/**
 * A byte array a syscall reads into.
 *
 * <p>The size comes from the program, so it may be larger than the heap. Asking
 * the JVM for such an array throws {@link OutOfMemoryError}, which is not an EO
 * termination and therefore passes every fallback by. The size is checked here
 * instead, the way {@code write} and {@code send} check the size against the
 * buffer they were given.</p>
 *
 * @since 0.64.0
 */
final class Buffer {

    /**
     * What the size is, for the failure message.
     */
    private final String subject;

    /**
     * How many bytes are wanted.
     */
    private final int size;

    /**
     * Ctor.
     * @param subject What the size is, for the failure message
     * @param size How many bytes are wanted
     */
    Buffer(final String subject, final int size) {
        this.subject = subject;
        this.size = size;
    }

    /**
     * Make it.
     * @return The array
     */
    byte[] it() {
        final Runtime runtime = Runtime.getRuntime();
        final long free = Math.min(
            runtime.maxMemory() - runtime.totalMemory() + runtime.freeMemory(),
            Integer.MAX_VALUE - 8L
        );
        if (this.size > free) {
            throw new ExFailure(
                "Can't allocate %d bytes for %s, while only %d bytes are available",
                this.size, this.subject, free
            );
        }
        return new byte[this.size];
    }
}
