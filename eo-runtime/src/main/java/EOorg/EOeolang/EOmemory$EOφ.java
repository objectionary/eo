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

import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * Memory.φ object.
 * @since 0.36.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "memory.@")
final class EOmemory$EOφ extends PhDefault implements Atom {
    /**
     * Ctor.
     * @param sigma Sigma
     */
    EOmemory$EOφ(final Phi sigma) {
        super(sigma);
    }

    @Override
    public Phi lambda() throws Exception {
        final byte[] bytes = new Dataized(this.take(Attr.RHO).take("data")).take();
        final Phi malloc = Phi.Φ.take("org.eolang.malloc").copy();
        malloc.put("size", new Data.ToPhi((long) bytes.length));
        final Phi pointer = malloc.take(Attr.PHI).take(Attr.LAMBDA);
        final Phi write = pointer.take("write").copy();
        write.put("offset", new Data.ToPhi(0L));
        write.put("data", new Data.ToPhi(bytes));
        new Dataized(write).take();
        final Phi alloc = this.take(Attr.SIGMA).take("allocated").copy();
        alloc.put("pointer", pointer);
        return alloc;
    }
}
