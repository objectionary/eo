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

/**
 * MAPI.
 *
 * @since 1.0
 */
public class EOarray$EOmapi extends PhDefault {

    public EOarray$EOmapi(final Phi parent) {
        super(parent);
        this.add("f", new AtFree());
        this.add("φ", new AtBound(new AtLambda(this, self -> {
            final Phi[] array = new Datarized(
                self.attr("ρ").get()
            ).take(Phi[].class);
            final Phi[] dest = new Phi[array.length];
            for (int idx = 0; idx < array.length; ++idx) {
                final Phi after = self.attr("f").get().copy();
                after.attr(0).put(array[idx]);
                after.attr(1).put(new Data.ToPhi((long)idx));
                dest[idx] = after;
            }
            return new Data.ToPhi(dest);
        })));
    }

}
