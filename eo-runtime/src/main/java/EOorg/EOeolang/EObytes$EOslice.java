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
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang; // NOPMD

import java.util.Arrays;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Expect;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * BYTES.SLICE.
 *
 * @since 1.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "bytes.slice")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EObytes$EOslice extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EObytes$EOslice() {
        this.add("start", new AtVoid("start"));
        this.add("len", new AtVoid("len"));
    }

    @Override
    public Phi lambda() {
        final byte[] array = new Dataized(this.take(Attr.RHO)).take();
        final int start = Expect.at(this, "start")
            .that(phi -> new Dataized(phi).asNumber())
            .otherwise("must be a number")
            .must(number -> number % 1 == 0)
            .that(Double::intValue)
            .otherwise("must be an integer")
            .must(integer -> integer >= 0)
            .otherwise("must be a positive integer")
            .must(integer -> integer < array.length)
            .otherwise(String.format("must be smaller than %d", array.length))
            .it();
        final int length = Expect.at(this, "len")
            .that(phi -> new Dataized(phi).asNumber())
            .otherwise("must be a number")
            .must(number -> number % 1 == 0)
            .that(Double::intValue)
            .otherwise("must be an integer")
            .must(integer -> integer >= 0)
            .otherwise("must be a positive integer")
            .must(integer -> integer < 1 + array.length - start)
            .otherwise(String.format("must be smaller than %d", 1 + array.length - start))
            .it();
        return new Data.ToPhi(
            Arrays.copyOfRange(array, start, start + length)
        );
    }
}
