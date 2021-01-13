/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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
import org.eolang.phi.PhDefault;
import org.eolang.phi.PhWith;
import org.eolang.phi.Phi;

/**
 * AND.
 *
 * @since 1.0
 */
public class EObool$EOand extends PhDefault {

    public EObool$EOand(final Phi parent) {
        super(parent);
        this.add("x", new AtFree());
        this.add("_origin", new AtBound(new AtLambda(this, self -> {
            final Boolean term = new Data.Take(
                self.attr("_parent").get()
            ).take(Boolean.class);
            final Boolean val = new Data.Take(
                self.attr("x").get()
            ).take(Boolean.class);
            return new PhWith(
                new org.eolang.EObool(),
                "data",
                new Data.Value<>(term && val)
            );
        })));
    }

}
