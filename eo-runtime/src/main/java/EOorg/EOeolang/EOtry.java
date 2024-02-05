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
import org.eolang.Atom;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * TRY.
 *
 * @since 0.19
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "try")
public final class EOtry extends PhDefault implements Atom {

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOtry(final Phi sigma) {
        super(sigma);
        this.add("main", new AtFree());
        this.add("catch", new AtFree());
        this.add("finally", new AtFree());
    }

    @Override
    public Phi lambda() {
        final Phi body = this.attr("main").get().copy();
        body.attr("ρ").put(this);
        Phi ret;
        try {
            ret = body;
            new Dataized(body).take();
        } catch (final EOerror.ExError ex) {
            final Phi ctch = this.attr("catch").get().copy();
            ctch.attr("ρ").put(this);
            ctch.attr(0).put(ex.enclosure());
            ret = ctch;
        } finally {
            final Phi fin = this.attr("finally").get().copy();
            fin.attr("ρ").put(this);
            new Dataized(fin).take();
        }
        return ret;
    }
}
