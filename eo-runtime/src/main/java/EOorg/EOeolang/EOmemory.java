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

import org.eolang.AtAtom;
import org.eolang.AtFree;
import org.eolang.AtMemoized;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * MEMORY.
 *
 * @since 1.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "memory")
public final class EOmemory extends PhDefault implements Atom {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOmemory(final Phi sigma) {
        super(sigma);
        this.add("enclosure", new AtMemoized());
        this.add("write", new EOmemory.AtMemoryWrite(this));
    }

    @Override
    public Phi lambda() {
        return this.attr("enclosure").get();
    }

    /**
     * Memory.write attribute.
     * @since 0.33.0
     */
    private static final class AtMemoryWrite extends AtAtom {
        /**
         * Ctor.
         * @param memory The {@link EOmemory} object
         */
        AtMemoryWrite(final Phi memory) {
            super(new EOmemory.Write(memory));
        }

        @Override
        public Attr copy(final Phi self) {
            return new AtMemoryWrite(self);
        }
    }

    /**
     * Memory write.
     * @since 1.0
     */
    @XmirObject(oname = "memory.write")
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
            final Attr enclosure = this.attr("σ").get().attr("enclosure");
            enclosure.put(this.attr("x").get());
            return enclosure.get();
        }
    }
}
