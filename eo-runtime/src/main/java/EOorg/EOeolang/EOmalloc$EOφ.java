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

import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Heaps;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * Malloc.φ object.
 * @since 0.36.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "malloc.@")
final class EOmalloc$EOφ extends PhDefault implements Atom {
    /**
     * Ctor.
     * @param sigma Sigma
     */
    EOmalloc$EOφ(final Phi sigma) {
        super(sigma);
        this.add("size", new AtVoid("size"));
    }

    @Override
    public Phi lambda() {
        final Phi size = this.take("size");
        final int identifier = Heaps.INSTANCE.get().malloc(
            this, new Dataized(size).take(Long.class).intValue()
        );
        final Phi pointer = this.take(Attr.RHO).take("pointer").copy();
        pointer.put("id", new Data.ToPhi((long) identifier));
        pointer.put("size", size);
        return pointer;
    }
}
