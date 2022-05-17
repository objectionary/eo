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
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * ARRAY.EQ.
 *
 * @since 1.0
 */
@XmirObject(oname = "array.eq")
public class EOarray$EOeq extends PhDefault {

    public EOarray$EOeq(final Phi sigma) {
        super(sigma);
        this.add("x", new AtFree());
        this.add("φ", new AtComposite(this, rho -> {
            final Phi[] array = new Param(rho).strong(Phi[].class);
            final Object expected = new Param(rho ,"x").weak();
            if (expected instanceof Object[]) {
                Phi[] comparable = (Phi[]) expected;
                if (!(comparable.length == array.length)) {
                    return new Data.ToPhi(false);
                }
                for (int idx = 0; idx < array.length; ++idx) {
                    if (!(new Dataized(array[idx]).take().equals(new Dataized(comparable[idx]).take()))) {
                        return new Data.ToPhi(false);
                    }
                }
                return new Data.ToPhi(true);
            }
            return new Data.ToPhi(false);
        }));
    }

}
