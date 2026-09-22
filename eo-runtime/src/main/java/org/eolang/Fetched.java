/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Optional;

/**
 * What {@link Heaps} answers a read request with.
 *
 * <p>It carries the bytes of the range, when the range fits inside the block,
 * and the size of the block they were checked against. Both are decided under
 * one hold of the lock, so a caller that has to explain a range that did not
 * fit never comes back to {@link Heaps} for that size, and a free running in
 * another thread cannot take the explanation away in between.</p>
 *
 * @since 0.77.0
 */
final class Fetched {

    /**
     * The bytes, if the range fits inside the block.
     */
    private final Optional<byte[]> data;

    /**
     * The size of the block the range was checked against.
     */
    private final int length;

    /**
     * Ctor.
     *
     * @param bytes The bytes, or nothing if the range lies outside the block
     * @param size How many bytes the block held when the range was checked
     */
    Fetched(final Optional<byte[]> bytes, final int size) {
        this.data = bytes;
        this.length = size;
    }

    /**
     * The bytes of the range.
     *
     * @return The bytes, or nothing if the range lies outside the block
     */
    Optional<byte[]> bytes() {
        return this.data;
    }

    /**
     * The size of the block.
     *
     * @return How many bytes the block held when the range was checked
     */
    int size() {
        return this.length;
    }
}
