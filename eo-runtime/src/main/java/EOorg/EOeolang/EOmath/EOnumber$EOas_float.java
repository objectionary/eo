/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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

package EOorg.EOeolang.EOmath;

import EOorg.EOeolang.EOerror;
import org.eolang.AtComposite;
import org.eolang.Data;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * as-float.
 *
 * @since 0.23
 */
@XmirObject(oname = "number.as-float")
public class EOnumber$EOas_float extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOnumber$EOas_float(final Phi sigma) {
        super(sigma);
        this.add("φ", new AtComposite(this, rho -> {
            final Phi number = rho.attr("ρ").get();
            final Object obj = new Param(number, "n").weak();
            Phi phi;
            if (obj instanceof Double) {
                phi = new Data.ToPhi(obj);
            } else if (obj instanceof Long) {
                phi = new Data.ToPhi(((Long) obj).doubleValue());
            } else {
                phi = new PhWith(
                    new EOerror(Phi.Φ), "msg",
                    new Data.ToPhi(
                        String.format("Wrong number's %s argument in number.as-float operation", obj)
                    )
                );
            }
            return phi;
        }));
    }

}
