/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import org.eolang.ExFailure;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * AT.
 *
 * @since 1.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "tuple.at")
public class EOtuple$EOat extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOtuple$EOat(final Phi sigma) {
        super(sigma);
        this.add("i", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final Phi[] tuple = new Param(rho).strong(Phi[].class);
                    int idx = new Param(rho, "i").strong(Long.class).intValue();
                    if (idx >= 0 && tuple.length <= idx) {
                        throw new ExFailure(
                            "Can't #at(%d) the %dth element of the tuple, there are just %d of them",
                            idx, idx + 1, tuple.length
                        );
                    } else if (idx < 0 && tuple.length < Math.abs(idx)) {
                        throw new ExFailure(
                            "Can't #at(%d) the %dth element of the tuple",
                            idx, tuple.length + idx
                        );
                    } else if (idx < 0) {
                        idx = tuple.length + idx;
                    }
                    return tuple[idx];
                }
            )
        );
    }

}
