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
import org.eolang.Param;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * SLICE.
 *
 * @since 0.23
 */
@XmirObject(oname = "string.slice")
public class EOstring$EOslice extends PhDefault {

    public EOstring$EOslice(final Phi sigma) {
        super(sigma);
        this.add("start", new AtFree());
        this.add("len", new AtFree());
        this.add("φ", new AtComposite(this, rho -> {
            final String str = new Param(rho).strong(String.class);
            final int start = new Param(rho, "start").strong(Long.class).intValue();
            final int length = new Param(rho, "len").strong(Long.class).intValue();
            final int end = length + start;
            final String error = validate(start, end, str.length());
            if (!error.isEmpty()) {
                return new PhWith(
                    new EOerror(Phi.Φ), "msg", new Data.ToPhi(error)
                );
            }
            return new Data.ToPhi(str.substring(start, end));
        }));
    }

    /**
     * Validate parameters.
     * @param start Requested start
     * @param end Requested end
     * @param actual Actual length
     * @return Message with error or empty string
     */
    private String validate(final int start, final int end, final int actual) {
        String msg = "";
        if (start < 0) {
            msg = String.format(
                "Start index must be greater than 0 but was %d",
                start
            );
        }
        if (start > end) {
            msg = String.format(
                "End index must be greater or equal to start but was %d < %d",
                end, start
            );
        }
        if (end > actual) {
            msg = String.format(
                "Start index + length must not exceed string length but was %d > %d",
                end, actual
            );
        }
        return msg;
    }

}
