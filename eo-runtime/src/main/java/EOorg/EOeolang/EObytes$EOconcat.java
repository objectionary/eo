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
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * BYTES.CONCAT.
 *
 * @since 0.23
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "bytes.concat")
public final class EObytes$EOconcat extends PhDefault implements Atom {
    /**
     * Ctor.
     */
    public EObytes$EOconcat() {
        this.add("b", new AtVoid("b"));
    }

    @Override
    public Phi lambda() {
        final byte[] current = new Dataized(this.take(Attr.RHO)).take();
        final byte[] provided = new Dataized(this.take("b")).take();
        final byte[] dest = new byte[current.length + provided.length];
        System.arraycopy(current, 0, dest, 0, current.length);
        System.arraycopy(provided, 0, dest, current.length, provided.length);
        return new Data.ToPhi(dest);
    }
}
