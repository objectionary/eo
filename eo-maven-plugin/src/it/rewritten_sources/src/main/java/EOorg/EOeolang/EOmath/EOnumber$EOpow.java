/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2022 Max Trunnikov
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
 * SOFTWARE
 */
// @checkstyle PackageNameCheck (1 line)
package EOorg.EOeolang.EOmath;

import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * POW.
 *
 * @checkstyle TypeNameCheck (100 lines)
 * @since 0.23
 */
@XmirObject(oname = "number.pow")
public final class EOnumber$EOpow extends PhDefault {

    /**
     * Ctor.
     *
     * @param sigma The \sigma
     * @checkstyle BracketsStructureCheck (200 lines)
     */
    public EOnumber$EOpow(final Phi sigma) {
        super(sigma);
        this.add("x", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final Phi number = rho.attr("ρ").get();
                    final Object base = new Param(number, "n").weak();
                    final Phi phi;
                    if (base instanceof Double) {
                        final double self = (Double) base;
                        final double pow = new Param(rho, "x").strong(Double.class);
                        if (self == 0.0d && pow < 0.0d) {
                            throw new ExFailure(
                                "0.0 cannot be raised to a negative power"
                            );
                        } else {
                            phi = new Data.ToPhi(Math.pow(self, pow));
                        }
                    } else if (base instanceof Long) {
                        final long self = (Long) base;
                        final long pow = new Param(rho, "x").strong(Long.class);
                        if (self == 0L && pow < 0L) {
                            throw new ExFailure(
                                "0 cannot be raised to a negative power"
                            );
                        } else {
                            phi = new Data.ToPhi(((Double) Math.pow(self, pow)).longValue());
                        }
                    } else {
                        throw new ExFailure(
                            String.format(
                                "Wrong number's %s argument in number.div operation",
                                base
                            )
                        );
                    }
                    return new PhWith(
                        new EOnumber(Phi.Φ),
                        "n",
                        phi
                    );
                }
            )
        );
    }

}
