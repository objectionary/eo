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
 * <p>A number is eight bytes wide and nothing else, so a payload of any
 * other width denotes no number at all. Handing such bytes to
 * {@link BytesRaw#asNumber()} fails, and the caller here is the builder
 * of a failure message, where a second failure buries the first one.
 * The caller gets an empty result for them and is expected to print the
 * structural form instead, the way {@link Quoted} answers for text that
 * is not UTF-8.</p>
 *
 * @since 0.77.0
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
