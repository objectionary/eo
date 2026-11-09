/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.sys;

import java.util.Arrays;
import java.util.function.ToIntBiFunction;
import org.eolang.Data;
import org.eolang.Expect;
import org.eolang.Natural;
import org.eolang.Phi;

/**
 * What a call that reads into a buffer hands back, as a {@code read-return}.
 *
 * <p>The size is agreed before the library is reached: {@link Natural} turns
 * down a negative or fractional one and {@link Buffer} one the heap cannot
 * hold. The call then fills the buffer and says how much of it it filled, and
 * only that much comes back as the data, so a failed call, which fills
 * nothing, hands back no bytes rather than a buffer of zeros.</p>
 *
 * <p>Public because {@code read} and {@code recv} of posix and of win32 all
 * read the same way, each from an atom of its own.</p>
 *
 * @since 0.77.0
 */
public final class Intake {

    /**
     * How many bytes to read.
     */
    private final Expect<Phi> size;

    /**
     * The {@code read-return} of the platform, still empty.
     */
    private final Phi forma;

    /**
     * The C function filling the buffer, telling how many bytes it filled.
     */
    private final ToIntBiFunction<byte[], Integer> call;

    /**
     * Ctor.
     *
     * @param size How many bytes to read
     * @param forma The {@code read-return} of the platform, still empty
     * @param call The C function filling the buffer
     */
    public Intake(
        final Expect<Phi> size, final Phi forma, final ToIntBiFunction<byte[], Integer> call
    ) {
        this.size = size;
        this.forma = forma;
        this.call = call;
    }

    /**
     * Return it.
     *
     * @return A copy of the {@code read-return}, filled
     */
    public Phi it() {
        final int length = new Natural(this.size).it();
        final byte[] buffer = new Buffer("the 'size' attribute", length).it();
        final int count = this.call.applyAsInt(buffer, length);
        final Phi result = this.forma.copy();
        result.put(0, new Data.ToPhi(count));
        result.put(1, new Data.ToPhi(Arrays.copyOf(buffer, Math.max(count, 0))));
        return result;
    }
}
