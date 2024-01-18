/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

import org.eolang.AtFree;
import org.eolang.AtLambda;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhConst;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * SEQ.
 * @checkstyle TypeNameCheck (5 lines)
 * @since 1.0
 */
@Versionized
@XmirObject(oname = "seq")
public class EOseq extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOseq(final Phi sigma) {
        super(sigma);
        this.add("steps", new AtFree());
        this.add(
            Attr.LAMBDA,
            new AtLambda(
                this,
                self -> {
                    final Phi args = new PhConst(self.attr("steps").get());
                    final Long length = new Dataized(
                        args.attr("length").get()
                    ).take(Long.class);
                    for (long idx = 0; idx < length - 1; ++idx) {
                        new Dataized(
                            new PhWith(
                                args.attr("at").get().copy(),
                                0, new Data.ToPhi(idx)
                            )
                        ).take();
                    }
                    final Phi ret;
                    if (length > 0) {
                        final Phi last = args.attr("at").get().copy();
                        last.attr(0).put(new Data.ToPhi(length - 1));
                        ret = last;
                    } else {
                        ret = new Data.ToPhi(false);
                    }
                    return ret;
                }
            )
        );
    }
}
