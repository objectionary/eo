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
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * TRY.
 *
 * @since 0.19
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "try")
public class EOtry extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOtry(final Phi sigma) {
        super(sigma);
        this.add("main", new AtFree());
        this.add("catch", new AtFree());
        this.add("finally", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final Phi body = rho.attr("main").get().copy();
                    body.attr("ρ").put(rho);
                    Phi ret;
                    try {
                        final Object obj = new Dataized(body).take();
                        ret = new Data.ToPhi(obj);
                    } catch (final EOerror.ExError ex) {
                        final Phi ctch = rho.attr("catch").get().copy();
                        ctch.attr("ρ").put(rho);
                        ctch.attr(0).put(ex.enclosure());
                        ret = new Data.ToPhi(new Dataized(ctch).take());
                    } finally {
                        final Phi fin = rho.attr("finally").get().copy();
                        fin.attr("ρ").put(rho);
                        new Dataized(fin).take();
                    }
                    return ret;
                }
            )
        );
    }

}
