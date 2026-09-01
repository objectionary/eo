/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Arrays;

/**
 * The padding a C {@code sockaddr_in} ends with.
 *
 * <p>The structure is sixteen bytes long whatever the platform lays out in
 * front of that padding, so the padding itself is always eight bytes. Anything
 * else changes the size of the structure a native socket call receives, and
 * an empty one is refused by JNA later on with a message about arrays of
 * length zero, far from the caller that gave it. {@link #bytes()} answers the
 * padding only when it is the right length, so there is no way to hold an
 * unchecked one.</p>
 *
 * @since 0.75
 */
final class Padding {

    /**
     * How many bytes of padding a {@code sockaddr_in} carries.
     */
    private static final int SIZE = 8;

    /**
     * The bytes given.
     */
    private final byte[] given;

    /**
     * Ctor.
     * @param bytes The bytes given
     */
    Padding(final byte[] bytes) {
        this.given = Arrays.copyOf(bytes, bytes.length);
    }

    /**
     * The padding, when it is the right length.
     * @return The bytes of the padding
     */
    byte[] bytes() {
        if (this.given.length != Padding.SIZE) {
            throw new IllegalArgumentException(
                String.format(
                    "The sockaddr_in padding must contain exactly %d bytes, not %d",
                    Padding.SIZE, this.given.length
                )
            );
        }
        return Arrays.copyOf(this.given, this.given.length);
    }
}
