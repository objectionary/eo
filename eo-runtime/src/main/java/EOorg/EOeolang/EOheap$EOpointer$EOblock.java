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

import java.util.Arrays;
import org.eolang.AtAtom;
import org.eolang.AtFree;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * HEAP.POINTER.BLOCK.
 *
 * @since 0.19
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "heap.pointer.block")
public final class EOheap$EOpointer$EOblock extends PhDefault implements Atom {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOheap$EOpointer$EOblock(final Phi sigma) {
        super(sigma);
        this.add("len", new AtFree());
        this.add("inverse", new AtFree());
        this.add("write", new AtWrite(this));
    }

    @Override
    public Phi lambda() {
        final Phi pointer = this.attr("σ").get();
        final int address =
            new Param(pointer, "address").strong(Long.class).intValue();
        final int len = new Param(this, "len").strong(Long.class).intValue();
        final byte[] chunk = Arrays.copyOfRange(
            Heaps.INSTANCE.get().data(pointer),
            address, address + len
        );
        final Phi inverse = this.attr("inverse").get().copy();
        inverse.attr("ρ").put(this);
        return new PhWith(inverse, 0, new Data.ToPhi(chunk));
    }

    /**
     * Head.pointer.block.write attribute.
     * @since 0.33.0
     */
    private static final class AtWrite extends AtAtom {
        /**
         * Ctor.
         * @param block The {@link EOheap$EOpointer$EOblock} object
         */
        AtWrite(final Phi block) {
            super(new EOheap$EOpointer$EOblock.Write(block));
        }

        @Override
        public Attr copy(final Phi self) {
            return new AtWrite(self);
        }
    }

    /**
     * Write block.
     * @since 0.19
     */
    @XmirObject(oname = "heap.pointer.block.write")
    private static final class Write extends PhDefault implements Atom {
        /**
         * Ctor.
         * @param sigma Sigma
         */
        Write(final Phi sigma) {
            super(sigma);
            this.add("x", new AtFree());
        }

        @Override
        public Phi lambda() {
            final Phi block = this.attr("σ").get();
            final Phi pointer = block.attr("σ").get();
            final int address =
                new Param(pointer, "address").strong(Long.class).intValue();
            final byte[] source = new Param(this, "x").strong(byte[].class);
            final byte[] data = Heaps.INSTANCE.get().data(pointer);
            System.arraycopy(source, 0, data, address, source.length);
            return new Data.ToPhi(true);
        }
    }

}
