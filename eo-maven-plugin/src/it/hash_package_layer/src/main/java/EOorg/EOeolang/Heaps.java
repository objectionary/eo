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
 */
package EOorg.EOeolang;

import java.util.concurrent.ConcurrentHashMap;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.eolang.Versionized;

/**
 * All heaps.
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
    private final ConcurrentHashMap<Phi, byte[]> all =
        new ConcurrentHashMap<>(0);

    /**
     * Heads.
     */
    private final ConcurrentHashMap<Phi, Integer> heads =
        new ConcurrentHashMap<>(0);

    /**
     * Ctor.
     */
    private Heaps() {
        // intentionally empty, it's a singleton :(
    }

    /**
     * Data.
     * @param pointer Pointer
     * @return Bytes comprising data
     */
    public byte[] data(final Phi pointer) {
        final Phi heap = pointer.attr("ρ").get();
        return this.all.computeIfAbsent(
            heap,
            key -> {
                final long size = new Dataized(heap.attr("size").get()).take(Long.class);
                if (size > (long) Integer.MAX_VALUE) {
                    throw new IllegalArgumentException(
                        String.format(
                            "The size of heap %d is too big",
                            size
                        )
                    );
                }
                return new byte[(int) size];
            }
        );
    }

    /**
     * Allocate a piece.
     *
     * The implementation is very primitive. It just allocates blocks
     * of memory one after another. It doesn't listen to free()
     * requests and never frees any blocks. They just go one by one.
     *
     * @param heap The heap
     * @param size How many bytes?
     * @return The pointer to it
     */
    public int malloc(final Phi heap, final int size) {
        synchronized (this.heads) {
            this.heads.putIfAbsent(heap, 0);
            final int ptr = this.heads.get(heap);
            this.heads.put(heap, ptr + size);
            return ptr;
        }
    }

    /**
     * Free it.
     * @param heap The heap
     * @param ptr The pointer
     * @checkstyle NonStaticMethodCheck (5 lines)
     */
    public void free(final Phi heap, final int ptr) {
        // Nothing yet
    }

}
