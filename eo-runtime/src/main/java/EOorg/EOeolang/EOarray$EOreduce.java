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
import org.eolang.AtFree;
import org.eolang.AtLambda;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * REDUCE.
 *
 * @since 1.0
 */
public class EOarray$EOreduce extends PhDefault {

    public EOarray$EOreduce(final Phi parent) {
        super(parent);
        this.add("a", new AtFree());
        this.add("f", new AtFree());
        this.add("φ", new AtBound(new AtLambda(this, self -> {
            final Phi[] array = new Dataized(
                self.attr("ρ").get()
            ).take(Phi[].class);
            Phi out = self.attr("a").get();
            for (final Phi arg : array) {
                final Phi after = self.attr("f").get().copy();
                after.attr(0).put(out);
                after.attr(1).put(arg);
                out = after;
            }
            return out;
        })));
    }

}
