/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.IntFunction;

/**
 * Dynamic memory.
 * @since 0.19
 * @todo #6507:30min Move the remaining tests onto the scoped malloc and make free private.
 */
final class Heaps {

    /**
     * EOorg.EOeolang.Heaps.
     */
    static final Heaps INSTANCE = new Heaps();

    /**
     * All.
     */
    @SuppressWarnings("PMD.LooseCoupling")
    private final ConcurrentHashMap<Integer, byte[]> blocks;

    /**
     * Lock.
     */
    private final Lock lock;

    /**
     * Ctor.
     */
    private Heaps() {
        this.blocks = new ConcurrentHashMap<>(0);
        this.lock = new ReentrantLock();
    }

    /**
     * Allocate a block in memory.
     * @param phi Object
     * @param size How many bytes
     * @return The identifier of pointer to the block in memory
     */
    int malloc(final Phi phi, final int size) {
        final int identifier = phi.hashCode();
        this.lock.lock();
        try {
            if (this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    String.format(
                        "Can't allocate block in memory with identifier '%d' because it's already allocated",
                        identifier
                    )
                );
            }
            this.blocks.put(identifier, new byte[size]);
        } finally {
            this.lock.unlock();
        }
        return identifier;
    }

    /**
     * Allocate a block in memory, let the scope use it, and free it afterwards.
     * @param phi Object
     * @param size How many bytes
     * @param scope What to do with the identifier of the block
     * @param <T> Type of what the scope returns
     * @return What the scope returns
     */
    <T> T malloc(final Phi phi, final int size, final IntFunction<T> scope) {
        final int identifier = this.malloc(phi, size);
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
                    String.format(
                        "Block in memory by identifier '%d' is not allocated, can't get size",
                        identifier
                    )
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
                String.format(
                    "Can't change size of block in memory by identifier '%d' to negative '%d'",
                    identifier, size
                )
            );
        }
        this.lock.lock();
        try {
            if (!this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    String.format(
                        "Block in memory by identifier '%d' is not allocated, can't get size",
                        identifier
                    )
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
                    String.format(
                        "Block in memory by identifier '%d' is not allocated, can't read",
                        identifier
                    )
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
            if (!this.fits(identifier, offset, length)) {
                throw new ExFailure(
                    String.format(
                        "Can't read '%d' bytes from offset '%d', because only '%d' are allocated",
                        length,
                        offset,
                        this.blocks.get(identifier).length
                    )
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
                    String.format(
                        "Can't read a block in memory with identifier '%d' because it's not allocated",
                        identifier
                    )
                );
            }
            if (offset < 0) {
                throw new ExFailure(
                    String.format(
                        "Block '%d': can't write at negative offset '%d'",
                        identifier, offset
                    )
                );
            }
            final long end = (long) offset + data.length;
            if (end > Integer.MAX_VALUE) {
                throw new ExFailure(
                    String.format(
                        "Block '%d': can't write at offset '%d', resulting size '%d' is too large for int",
                        identifier, offset, end
                    )
                );
            }
            if (this.blocks.get(identifier).length < end) {
                this.resize(identifier, (int) end);
            }
            final byte[] source = this.blocks.get(identifier);
            final int length = source.length;
            final byte[] result = new byte[length];
            System.arraycopy(source, 0, result, 0, length);
            System.arraycopy(data, 0, result, offset, data.length);
            this.blocks.put(identifier, result);
        } finally {
            this.lock.unlock();
        }
    }

    /**
     * Free it.
     * @param identifier Identifier of pointer
     */
    void free(final int identifier) {
        this.lock.lock();
        try {
            if (!this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    String.format(
                        "Can't free a block in memory with identifier '%d' because it's not allocated",
                        identifier
                    )
                );
            }
            this.blocks.remove(identifier);
        } finally {
            this.lock.unlock();
        }
    }
}
