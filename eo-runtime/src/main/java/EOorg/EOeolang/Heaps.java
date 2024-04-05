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

package EOorg.EOeolang;

import java.util.concurrent.ConcurrentHashMap;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.eolang.Versionized;

/**
 * Dynamic memory.
 *
 * @since 0.19
 */
@Versionized
final class Heaps {

    /**
     * Heaps.
     */
    public static final ThreadLocal<Heaps> INSTANCE = ThreadLocal.withInitial(Heaps::new);

    /**
     * All.
     */
    private final ConcurrentHashMap<Integer, byte[]> blocks =
        new ConcurrentHashMap<>(0);

    /**
     * Ctor.
     */
    private Heaps() {
        // intentionally empty, it's a singleton :(
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
                        "Can't allocate block in memory with identifier %d because it's already allocated",
                        identifier
                    )
                );
            }
            this.blocks.put(identifier, new byte[size]);
        }
        return identifier;
    }

    /**
     * Get data from the block in memory by identifier.
     * @param identifier Identifier of the pointer
     * @return Bytes from the block in memory
     */
    byte[] read(final int identifier) {
        synchronized (this.blocks) {
            if (!this.blocks.containsKey(identifier)) {
                throw new ExFailure(
                    String.format(
                        "Block in memory by identifier %d is not allocated, can't read",
                        identifier
                    )
                );
            }
            return this.blocks.get(identifier);
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
                        "Can't read a block in memory with identifier %d because it's not allocated",
                        identifier
                    )
                );
            }
            final byte[] current = this.blocks.get(identifier);
            final int length = current.length;
            if (length < offset + data.length) {
                throw new ExFailure(
                    String.format(
                        "Can't write %d bytes with offset %d to the block with identifier %d, because only %d were allocated",
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
                        "Can't free a block in memory with identifier %d because it's not allocated",
                        identifier
                    )
                );
            }
            this.blocks.remove(identifier);
        }
    }
}
