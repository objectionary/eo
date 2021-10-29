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

package EOorg.EOeolang;

import org.eolang.AtBound;
import org.eolang.AtLambda;
import org.eolang.AtVararg;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * AND.
 *
 * @since 1.0
 */
public class EObool$EOor extends PhDefault {

    public EObool$EOor(final Phi parent, final EObool up) {
        super(parent);
        this.add("x", new AtVararg());
        this.add("φ", new AtBound(new AtLambda(this, self -> {
            Boolean term = new Dataized(
                self.attr("ρ").get()
            ).take(Boolean.class);
            final Phi[] args = new Dataized(
                self.attr("x").get()
            ).take(Phi[].class);
            for (final Phi arg : args) {
                if (term) {
                    break;
                }
                term |= new Dataized(arg).take(Boolean.class);
            }
            return new Data.ToPhi(term);
        })));
    }

}
