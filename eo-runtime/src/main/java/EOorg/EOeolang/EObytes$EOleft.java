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

import org.eolang.*;
import java.math.BigInteger;

/**
 * BYTES.LEFT.
 *
 * @since 1.0
 */
@XmirObject(oname = "bytes.left")
public class EObytes$EOleft extends PhDefault {

    public EObytes$EOleft(final Phi sigma) {
        super(sigma);
        this.add("x", new AtFree());
        this.add("φ", new AtComposite(this, self -> {
            final byte[] array = new Dataized(
                self.attr("ρ").get()
            ).take(byte[].class);
            byte[] result = new BigInteger(array).shiftLeft(
                new Dataized(self.attr("x").get())
                .take(Long.class).intValue()
            ).toByteArray();
            int newLength = Math.max(array.length, result.length);
            final byte[] output = new byte[newLength];
            System.arraycopy(result, 0, output, newLength  - result.length, result.length);
            return new Data.ToPhi(output);
        }));
    }

}
