/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.IntFunction;

/**
 * Dynamic memory.
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
     * Lock.
     */
    private final Lock lock;

    /**
     * Next identifier to hand out.
     */
    private final AtomicInteger next;

    /**
     * Ctor.
     */
    private Heaps() {
        this.blocks = new ConcurrentHashMap<>(0);
        this.lock = new ReentrantLock();
        this.next = new AtomicInteger();
    }

    /**
     * Allocate a block in memory, let the scope use it, and free it afterwards.
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
     * @param identifier Identifier of block in memory
     * @return Size
     */
    int size(final int identifier) {
        this.lock.lock();
        try {
            if (!this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    "Block in memory by identifier '%d' is not allocated, can't get size",
                    identifier
                );
            }
            return this.blocks.get(identifier).length;
        } finally {
            this.lock.unlock();
        }
    }

    /**
     * Resize allocated block in memory.
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
        this.lock.lock();
        try {
            if (!this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    "Block in memory by identifier '%d' is not allocated, can't get size",
                    identifier
                );
            }
            final byte[] bytes = this.blocks.get(identifier);
            final byte[] resized = new byte[size];
            System.arraycopy(bytes, 0, resized, 0, Math.min(bytes.length, size));
            this.blocks.put(identifier, resized);
        } finally {
            this.lock.unlock();
        }
    }

    /**
     * Whether the given range fits inside the allocated block — the
     * single source of truth for the read-bounds rule.
     *
     * <p>If the block is not allocated, the request is a structural
     * (unpredictable) failure and aborts with {@link ExFailure}, which
     * EO cannot catch. A range that exceeds an allocated block is a
     * predictable failure, reported as {@code false} so the caller can
     * fall back rather than read garbage.</p>
     *
     * @param identifier Identifier of the block
     * @param offset Offset to start reading from
     * @param length Length of bytes to read
     * @return True if the range lies within the allocated block
     */
    boolean fits(final int identifier, final int offset, final int length) {
        this.lock.lock();
        try {
            if (!this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    "Block in memory by identifier '%d' is not allocated, can't read",
                    identifier
                );
            }
            return offset >= 0
                && length >= 0
                && (long) offset + length <= this.blocks.get(identifier).length;
        } finally {
            this.lock.unlock();
        }
    }

    /**
     * Get data from the block in memory by identifier.
     * @param identifier Identifier of the pointer
     * @param offset Offset to start reading from
     * @param length Length of bytes to read
     * @return Bytes from the block in memory
     */
    byte[] read(final int identifier, final int offset, final int length) {
        this.lock.lock();
        try {
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
            if (!this.fits(identifier, offset, length)) {
                throw new ExFailure(
                    "Can't read '%d' bytes from offset '%d', because only '%d' are allocated",
                    length,
                    offset,
                    this.blocks.get(identifier).length
                );
            }
            return Arrays.copyOfRange(this.blocks.get(identifier), offset, offset + length);
        } finally {
            this.lock.unlock();
        }
    }

    /**
     * Write given data to the block in memory by given identifier.
     * @param identifier Identifier of the pointer
     * @param offset Writing offset
     * @param data Data to write
     */
    void write(final int identifier, final int offset, final byte[] data) {
        this.lock.lock();
        try {
            if (!this.blocks.containsKey(identifier)) {
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
            final byte[] source = this.blocks.get(identifier);
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
        } finally {
            this.lock.unlock();
        }
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
        this.lock.lock();
        try {
            this.blocks.put(identifier, new byte[size]);
        } finally {
            this.lock.unlock();
        }
        return identifier;
    }

    private void free(final int identifier) {
        this.lock.lock();
        try {
            if (!this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    "Can't free a block in memory with identifier '%d' because it's not allocated",
                    identifier
                );
            }
            this.blocks.remove(identifier);
        } finally {
            this.lock.unlock();
        }
    }
}
