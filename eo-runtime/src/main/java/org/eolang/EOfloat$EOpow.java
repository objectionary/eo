/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

package org.eolang;

import org.eolang.phi.AtBound;
import org.eolang.phi.AtFree;
import org.eolang.phi.AtLambda;
import org.eolang.phi.Data;
import org.eolang.phi.Datarized;
import org.eolang.phi.PhDefault;
import org.eolang.phi.Phi;
import org.eolang.phi.PhWith;

/**
 * POW.
 *
 * @since 1.0
 */
public class EOfloat$EOpow extends PhDefault {

    public EOfloat$EOpow(final Phi parent) {
        super(parent);
        this.add("x", new AtFree());
        this.add("φ", new AtBound(new AtLambda(this, self -> {
            double ρ = new Datarized(self.attr("ρ").get()).take(Double.class);
            double x = new Datarized(self.attr("x").get()).take(Double.class);
            return new Data.ToPhi(Math.pow(ρ, x));
        })));
    }

}
