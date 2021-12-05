/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

package EOorg.EOeolang.EOgray;

import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * HEAP.POINTER.BLOCK.
 *
 * @since 0.19
 */
@XmirObject(oname = "heap.pointer.block")
public class EOheap$EOpointer$EOblock extends PhDefault {

    private static final ConcurrentHashMap<Phi, byte[]> HEAPS =
        new ConcurrentHashMap<>(0);

    public EOheap$EOpointer$EOblock(final Phi sigma) {
        super(sigma);
        this.add("len", new AtFree());
        this.add("inverse", new AtFree());
        this.add("write", new AtComposite(this, EOheap$EOpointer$EOblock.Write::new));
        this.add("φ", new AtComposite(this, rho -> {
            final Phi pointer = rho.attr("ρ").get();
            final int address = new Dataized(
                pointer.attr("address").get()
            ).take(Long.class).intValue();
            final int len = new Dataized(
                rho.attr("len").get()
            ).take(Long.class).intValue();
            final byte[] chunk = Arrays.copyOfRange(
                EOheap$EOpointer$EOblock.data(pointer),
                address, address + len
            );
            final Phi inverse = rho.attr("inverse").get();
            return new PhWith(inverse, 0, new Data.ToPhi(chunk));
        }));
    }

    @XmirObject(oname = "heap.pointer.block.write")
    private final class Write extends PhDefault {
        Write(final Phi sigma) {
            super(sigma);
            this.add("x", new AtFree());
            this.add("φ", new AtComposite(this, rho -> {
                final Phi block = rho.attr("ρ").get();
                final Phi pointer = block.attr("ρ").get();
                final int address = new Dataized(
                    pointer.attr("address").get()
                ).take(Long.class).intValue();
                final byte[] src = new Dataized(
                    rho.attr("x").get()
                ).take(byte[].class);
                final byte[] data = EOheap$EOpointer$EOblock.data(pointer);
                System.arraycopy(src, 0, data, address, src.length);
                return new Data.ToPhi(true);
            }));
        }
    }
    private static byte[] data(final Phi pointer) {
        final Phi heap = pointer.attr("ρ").get();
        return EOheap$EOpointer$EOblock.HEAPS.computeIfAbsent(
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
}
