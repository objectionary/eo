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

package EOorg.EOeolang;

import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.AtVararg;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * DIV.
 *
 * @since 1.0
 */
@XmirObject(oname = "int.div")
public class EOint$EOdiv extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOint$EOdiv(final Phi sigma) {
        super(sigma);
        this.add("x", new AtVararg());
        this.add("φ", new AtComposite(this, rho -> {
            Phi phi = null;
            boolean error = false;
            Long div = new Param(rho).strong(Long.class);
            final Phi[] args = new Param(rho, "x").strong(Phi[].class);
            for (int idx = 0; idx < args.length; ++idx) {
                final Object val = new Dataized(args[idx]).take();
                if (!(val instanceof Long)) {
                    phi = new PhWith(
                        new EOerror(Phi.Φ),
                        "msg",
                        new Data.ToPhi(
                            String.format(
                                "The %dth argument of 'div' is not an int: %s",
                                idx + 1, val
                            )
                        )
                    );
                    error = true;
                    break;
                }
                long typed = (Long) val;
                if (typed == 0L) {
                    phi = new PhWith(
                        new EOerror(Phi.Φ),
                        "msg",
                        new Data.ToPhi(
                            String.format(
                                "Division by 0 at %dth argument of 'div'",
                                idx + 1
                            )
                        )
                    );
                    error = true;
                    break;
                }
                div /= (Long) val;
            }
            if (!error) {
                phi = new Data.ToPhi(div);
            }
            return phi;
        }));
    }
}
