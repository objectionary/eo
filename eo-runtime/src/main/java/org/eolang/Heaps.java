/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.IntFunction;

/**
 * Dynamic memory.
 *
 * @since 0.19
 */
final class Heaps {

    /**
     * EOorg.EOeolang.Heaps.
     */
    static final Heaps INSTANCE = new Heaps();

    /**
     * All.
     */
    private final ConcurrentMap<Integer, byte[]> blocks;



    /**
     * Next identifier to hand out.
     */
    private final AtomicInteger next;

    /**
     * Ctor.
     */
    private Heaps() {
        this.blocks = new ConcurrentHashMap<>(0);
        this.next = new AtomicInteger();
    }

    /**
     * Allocate a block in memory, let the scope use it, and free it afterwards.
     *
     * @param size How many bytes
     * @param scope What to do with the identifier of the block
     * @param <T> Type of what the scope returns
     * @return What the scope returns
     */
    <T> T malloc(final int size, final IntFunction<T> scope) {
        final int identifier = this.malloc(size);
        try {
            return scope.apply(identifier);
        } finally {
            this.free(identifier);
        }
    }

    /**
     * Get size of allocated block in memory by provided identifier.
     *
     * @param identifier Identifier of block in memory
     * @return Size
     */
    int size(final int identifier) {
        final byte[] block = this.blocks.get(identifier);
        if (block == null) {
            throw new ExFailure(
                    "Block in memory by identifier '%d' is not allocated, can't get size",
                    identifier
            );
        }
        return block.length;
    }

    /**
     * Resize allocated block in memory.
     *
     * @param identifier Identifier of block
     * @param size New size
     */
    void resize(final int identifier, final int size) {
        if (size < 0) {
            throw new ExFailure(
                "Can't change size of block in memory by identifier '%d' to negative '%d'",
                identifier, size
            );
        }
        final byte[] modified = this.blocks.computeIfPresent(identifier, (id, bytes) -> Arrays.copyOf(bytes, size));
        if (modified == null) {
            throw new ExFailure(
                    "Block in memory by identifier '%d' is not allocated, can't get size",
                    identifier
            );
        }
    }

    /**
     * The bytes of the given range, if it fits inside the allocated block —
     * the single source of truth for the read-bounds rule.
     *
     * <p>If the block is not allocated, the request is a structural
     * (unpredictable) failure and aborts with {@link ExFailure}, which
     * EO cannot catch. A range that exceeds an allocated block is a
     * predictable failure, reported as an empty answer so the caller can
     * fall back rather than read garbage. The range is checked and copied
     * under one hold of the lock, so a resize cannot shrink the block
     * between the two and take the fallback away from the caller.</p>
     *
     * <p>The answer carries the size of the block as well, because a caller
     * that falls back has to say how many bytes were allocated. Asking for
     * that size afterwards would be a second, separately locked question,
     * and a free arriving between the two would abort it.</p>
     *
     * @param identifier Identifier of the block
     * @param offset Offset to start reading from
     * @param length Length of bytes to read
     * @return The bytes, or nothing if the range lies outside the block,
     *  together with the size of the block
     */
    Fetched fetched(final int identifier, final int offset, final int length) {
        final byte[] block = this.blocks.get(identifier);
        if (block == null) {
            throw new ExFailure(
                    "Block in memory by identifier '%d' is not allocated, can't read",
                    identifier
            );
        }
        final Optional<byte[]> out;
        if (offset >= 0 && length >= 0 && (long) offset + length <= block.length) {
            out = Optional.of(Arrays.copyOfRange(block, offset, offset + length));
        } else {
            out = Optional.empty();
        }
        return new Fetched(out, block.length);
    }

    /**
     * Get data from the block in memory by identifier.
     *
     * @param identifier Identifier of the pointer
     * @param offset Offset to start reading from
     * @param length Length of bytes to read
     * @return Bytes from the block in memory
     */
    byte[] read(final int identifier, final int offset, final int length) {
        if (offset < 0) {
            throw new ExFailure(
                    "Block '%d': can't read at negative offset '%d'",
                    identifier, offset
            );
        }
        if (length < 0) {
            throw new ExFailure(
                    "Block '%d': can't read a negative number of bytes '%d'",
                    identifier, length
            );
        }
        final Fetched data = this.fetched(identifier, offset, length);
        return data.bytes().orElseThrow(
                () -> new ExFailure(
                        "Can't read '%d' bytes from offset '%d', because only '%d' are allocated",
                        length,
                        offset,
                        data.size()
                )
        );
    }

    /**
     * Write given data to the block in memory by given identifier.
     *
     * @param identifier Identifier of the pointer
     * @param offset Writing offset
     * @param data Data to write
     */
    void write(final int identifier, final int offset, final byte[] data) {
        final byte[] source = this.blocks.get(identifier);
        if (source == null) {
            throw new ExFailure(
                    "Can't read a block in memory with identifier '%d' because it's not allocated",
                    identifier
            );
        }
        if (offset < 0) {
            throw new ExFailure(
                    "Block '%d': can't write at negative offset '%d'",
                    identifier, offset
            );
        }
        final long end = (long) offset + data.length;
        if (end > Integer.MAX_VALUE) {
            throw new ExFailure(
                    "Block '%d': can't write at offset '%d', resulting size '%d' is too large for int",
                    identifier, offset, end
            );
        }
        final int length = source.length;
        if (length < end) {
            throw new ExFailure(
                    "Can't write '%d' bytes with offset '%d' to the block with identifier '%d', because only '%d' were allocated",
                    data.length,
                    offset,
                    identifier,
                    length
            );
        }
        System.arraycopy(data, 0, source, offset, data.length);
    }

    private int malloc(final int size) {
        if (size < 0) {
            throw new ExFailure(
                "Can't allocate block in memory with negative size '%d'",
                size
            );
        }
        final int identifier = this.next.getAndIncrement();
        if (identifier < 0) {
            throw new ExFailure(
                "Can't allocate a block in memory, ran out of identifiers"
            );
        }
        this.blocks.put(identifier, new byte[size]);
        return identifier;
    }

    private void free(final int identifier) {
        if (this.blocks.remove(identifier) == null) {
            throw new ExFailure(
                    "Can't free a block in memory with identifier '%d' because it's not allocated",
                    identifier
            );
        }
    }
}
