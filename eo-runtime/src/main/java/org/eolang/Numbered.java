/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Arrays;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * Bytes rendered as an EO number literal.
 *
 * <p>A number is eight bytes wide and nothing else denotes one, so bytes
 * of any other width are refused here the way {@link Quoted} refuses
 * bytes that are not valid UTF-8. The caller gets an empty result and is
 * expected to print the structural form instead, since a renderer that
 * throws replaces the failure it was called to describe.</p>
 *
 * @since 0.75.0
 */
final class Numbered implements Supplier<Optional<String>> {

    /**
     * The bytes of the number.
     */
    private final byte[] data;

    /**
     * Ctor.
     *
     * @param data The bytes
     */
    Numbered(final byte[] data) {
        this.data = Arrays.copyOf(data, data.length);
    }

    @Override
    public Optional<String> get() {
        final Optional<String> result;
        if (this.data.length == Double.BYTES) {
            result = Optional.of(new Numeral(new BytesOf(this.data).asNumber()).get());
        } else {
            result = Optional.empty();
        }
        return result;
    }
}
