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

package org.eolang.txt;

import java.util.Collection;
import java.util.LinkedList;
import org.eolang.phi.AtBound;
import org.eolang.phi.AtFree;
import org.eolang.phi.AtLambda;
import org.eolang.phi.AtVararg;
import org.eolang.phi.Data;
import org.eolang.phi.Datarized;
import org.eolang.phi.PhDefault;
import org.eolang.phi.Phi;

/**
 * Sprintf.
 *
 * @since 0.2
 */
public class EOsprintf extends PhDefault {

    public EOsprintf(final Phi parent) {
        super(parent);
        this.add("format", new AtFree());
        this.add("args", new AtVararg());
        this.add("φ", new AtBound(new AtLambda(this, self -> {
            final String format = new Datarized(
                self.attr("format").get()
            ).take(String.class);
            final Phi[] args = new Datarized(
                self.attr("args").get()
            ).take(Phi[].class);
            final Collection<Object> items = new LinkedList<>();
            for (final Phi arg : args) {
                items.add(new Datarized(arg).take());
            }
            return new Data.ToPhi(String.format(format, items.toArray()));
        })));
    }

}
