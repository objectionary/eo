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

/*
 * @checkstyle PackageNameCheck (4 lines)
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
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "string.slice")
public class EOstring$EOslice extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOstring$EOslice(final Phi sigma) {
        super(sigma);
        this.add("start", new AtFree());
        this.add("len", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final String str = new Param(rho).strong(String.class);
                    final int start = new Param(rho, "start").strong(Long.class).intValue();
                    final int length = new Param(rho, "len").strong(Long.class).intValue();
                    final int end = length + start;
                    final Phi result;
                    if (start < 0) {
                        result = error(
                            "Start index must be greater than 0 but was %d",
                            start
                        );
                    } else if (start > end) {
                        result = error(
                            "End index must be greater or equal to start but was %d < %d",
                            end, start
                        );
                    } else if (end > str.length()) {
                        result = error(
                            "Start index + length must not exceed string length but was %d > %d",
                            end, str.length()
                        );
                    } else {
                        result = new Data.ToPhi(str.substring(start, end));
                    }
                    return result;
                }
            )
        );
    }

    /**
     * Building error.
     * @param msg Formatted string for error message
     * @param args Arguments for the formatted string
     * @return A φ containing error
     */
    private static Phi error(final String msg, final Object... args) {
        return new PhWith(
            new EOerror(Phi.Φ),
            "msg",
            new Data.ToPhi(
                String.format(msg, args)
            )
        );
    }
}
