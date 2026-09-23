/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang.sys;

import java.util.Arrays;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.Expect;
import org.eolang.Natural;
import org.eolang.Phi;

/**
 * The head of a buffer that a write is asked to hand to a descriptor.
 *
 * <p>A size no write could mean is refused here rather than on the native
 * side: {@link Natural} turns down a negative or fractional one, and a size
 * larger than the buffer it names is turned down too, since the C library
 * would then read past the end of what EO gave it. What comes back is the
 * head itself, so the caller has nothing left to measure and the library is
 * reached only once the size has been agreed.</p>
 *
 * <p>Public because posix and win32 ask the same question of the same two
 * attributes, each from an atom of its own.</p>
 *
 * @since 0.77.0
 */
public final class Portion {

    /**
     * The object holding the buffer and the size.
     */
    private final Phi phi;

    /**
     * Ctor.
     *
     * @param phi The object holding the buffer and the size
     */
    public Portion(final Phi phi) {
        this.phi = phi;
    }

    /**
     * Return it.
     *
     * @return The bytes to hand over
     */
    public byte[] it() {
        final byte[] buffer = new Dataized(this.phi.take("buffer")).take();
        final int size = new Natural(Expect.at(this.phi, "size")).it();
        if (size > buffer.length) {
            throw new ExFailure(
                "Can't write %d bytes from a buffer of only %d bytes",
                size, buffer.length
            );
        }
        return Arrays.copyOf(buffer, size);
    }
}
