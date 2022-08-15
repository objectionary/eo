/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * HEAP.FREE.
 *
 * @since 0.19
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "heap.free")
public class EOheap$EOfree extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOheap$EOfree(final Phi sigma) {
        super(sigma);
        this.add("p", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final Phi heap = rho.attr("ρ").get();
                    final int ptr = new Param(rho, "p").strong(Long.class).intValue();
                    Heaps.INSTANCE.free(heap, ptr);
                    return new Data.ToPhi(true);
                }
            )
        );
    }

}
