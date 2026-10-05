/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Arrays;

/**
 * A part of some bytes: the bytes that start at an offset and go on for a
 * length.
 *
 * <p>This is the Java that eo-lowering writes in the place of the atom
 * {@code bytes.slice}. It checks the offset and the length with
 * {@link Natural}, as the atom does, so a wrong one fails with the same
 * message. When the part goes past the end of the bytes, the atom returns
 * the {@code cant-slice} its caller binds, but no object of eo-runtime
 * binds it, so reading that unbound void fails with the message this
 * class fails with.</p>
 *
 * @since 0.64.0
 */
public final class Slice implements Data {

    /**
     * The bytes to take the part from.
     */
    private final byte[] bytes;

    /**
     * The offset of the first byte of the part.
     */
    private final double start;

    /**
     * The number of bytes in the part.
     */
    private final double len;

    /**
     * Ctor.
     *
     * @param bytes The bytes to take the part from
     * @param start The offset of the first byte of the part
     * @param len The number of bytes in the part
     */
    public Slice(final byte[] bytes, final double start, final double len) {
        this.bytes = Arrays.copyOf(bytes, bytes.length);
        this.start = start;
        this.len = len;
    }

    @Override
    public byte[] delta() {
        final int from = new Natural(
            new Expect<>("the 'start' attribute", () -> new Data.ToPhi(this.start))
        ).it();
        final int size = new Natural(
            new Expect<>("the 'len' attribute", () -> new Data.ToPhi(this.len))
        ).it();
        if ((long) from + size > this.bytes.length) {
            throw new ExFailure(
                "cannot slice '%d' bytes from offset '%d' of bytes of size %d",
                size, from, this.bytes.length
            );
        }
        return Arrays.copyOfRange(this.bytes, from, from + size);
    }
}
