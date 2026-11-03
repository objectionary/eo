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
 * The head of a buffer that a call is asked to hand to a descriptor.
 *
 * <p>A size no call could mean is refused here rather than on the native
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
     * The buffer.
     */
    private final Phi buffer;

    /**
     * How many bytes of the buffer to hand over.
     */
    private final Expect<Phi> size;

    /**
     * Ctor.
     *
     * @param buffer The buffer
     * @param size How many bytes of the buffer to hand over
     */
    public Portion(final Phi buffer, final Expect<Phi> size) {
        this.buffer = buffer;
        this.size = size;
    }

    /**
     * Return it.
     *
     * @return The bytes to hand over
     */
    public byte[] it() {
        final byte[] bytes = new Dataized(this.buffer).take();
        final int count = new Natural(this.size).it();
        if (count > bytes.length) {
            throw new ExFailure(
                "Can't take %d bytes from a buffer of only %d bytes",
                count, bytes.length
            );
        }
        return Arrays.copyOf(bytes, count);
    }
}
