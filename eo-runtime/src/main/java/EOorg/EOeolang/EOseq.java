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

import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhMethod;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * SEQ.
 * @since 1.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "seq")
public final class EOseq extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOseq() {
        this.add("steps", new AtVoid("steps"));
    }

    @Override
    public Phi lambda() {
        final Phi steps = this.take("steps");
        final Phi[] items = EOseq.eoTupleAsArray(steps);
        for (int ind = 0; ind < items.length - 1; ++ind) {
            new Dataized(items[ind]).take();
        }
        final Phi ret;
        if (items.length > 0) {
            ret = new PhMethod(steps, "tail");
        } else {
            ret = new Data.ToPhi(false);
        }
        return ret;
    }

    /**
     * Converts eo tuple to java array.
     * @param args Eo tuple.
     * @return Java array.
     */
    private static Phi[] eoTupleAsArray(final Phi args) {
        final int length = Math.toIntExact(
            new Dataized(
                args.take("length")
            ).take(Long.class)
        );
        final Phi[] res = new Phi[length];
        Phi external = args;
        for (int ind = length - 1; ind >= 0; --ind) {
            res[ind] = new PhMethod(external, "tail");
            external = external.take("head");
        }
        return res;
    }
}
