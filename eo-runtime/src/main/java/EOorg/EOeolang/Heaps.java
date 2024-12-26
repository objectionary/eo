/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import org.eolang.ExFailure;
import org.eolang.Phi;

/**
 * Dynamic memory.
 *
 * @since 0.19
 */
final class Heaps {

    /**
     * Heaps.
     */
    static final Heaps INSTANCE = new Heaps();

    /**
     * All.
     */
    private final ConcurrentHashMap<Integer, byte[]> blocks;

    /**
     * Ctor.
     */
    private Heaps() {
        this.blocks = new ConcurrentHashMap<>(0);
    }

    /**
     * Allocate a block in memory.
     * @param phi Object
     * @param size How many bytes
     * @return The identifier of pointer to the block in memory
     */
    int malloc(final Phi phi, final int size) {
        final int identifier = phi.hashCode();
        synchronized (this.blocks) {
            if (this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    String.format(
                        "Can't allocate block in memory with identifier '%d' because it's already allocated",
                        identifier
                    )
                );
            }
            this.blocks.put(identifier, new byte[size]);
        }
        return identifier;
    }

    /**
     * Get size of allocated block in memory by provided identifier.
     * @param identifier Identifier of block in memory
     * @return Size
     */
    int size(final int identifier) {
        synchronized (this.blocks) {
            if (!this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    String.format(
                        "Block in memory by identifier '%d' is not allocated, can't get size",
                        identifier
                    )
                );
            }
            return this.blocks.get(identifier).length;
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
        synchronized (this.blocks) {
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
        synchronized (this.blocks) {
            if (!this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    String.format(
                        "Block in memory by identifier '%d' is not allocated, can't read",
                        identifier
                    )
                );
            }
            final byte[] bytes = this.blocks.get(identifier);
            if (offset + length > bytes.length) {
                throw new ExFailure(
                    String.format(
                        "Can't read '%d' bytes from offset '%d', because only '%d' are allocated",
                        length,
                        offset,
                        bytes.length
                    )
                );
            }
            return Arrays.copyOfRange(bytes, offset, offset + length);
        }
    }

    /**
     * Write given data to the block in memory by given identifier.
     * @param identifier Identifier of the pointer
     * @param offset Writing offset
     * @param data Data to write
     */
    void write(final int identifier, final int offset, final byte[] data) {
        synchronized (this.blocks) {
            if (!this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    String.format(
                        "Can't read a block in memory with identifier '%d' because it's not allocated",
                        identifier
                    )
                );
            }
            final byte[] current = this.blocks.get(identifier);
            final int length = current.length;
            if (length < offset + data.length) {
                throw new ExFailure(
                    String.format(
                        "Can't write '%d' bytes with offset '%d' to the block with identifier '%d', because only '%d' were allocated",
                        data.length,
                        offset,
                        identifier,
                        length
                    )
                );
            }
            final byte[] result = new byte[length];
            if (offset > 0) {
                System.arraycopy(current, 0, result, 0, offset);
            }
            System.arraycopy(data, 0, result, offset, data.length);
            if (length > offset + data.length) {
                System.arraycopy(
                    current,
                    offset + data.length,
                    result,
                    offset + data.length,
                    length - offset - data.length
                );
            }
            this.blocks.put(identifier, result);
        }
    }

    /**
     * Free it.
     * @param identifier Identifier of pointer
     */
    void free(final int identifier) {
        synchronized (this.blocks) {
            if (!this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    String.format(
                        "Can't free a block in memory with identifier '%d' because it's not allocated",
                        identifier
                    )
                );
            }
            this.blocks.remove(identifier);
        }
    }
}
