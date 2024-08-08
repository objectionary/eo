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
package EOorg.EOeolang.EOsys; // NOPMD

import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * Unix syscall.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@Versionized
@XmirObject(oname = "posix.@")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOφ extends PhDefault implements Atom {

    @Override
    public Phi lambda() throws Exception {
        final Phi rho = this.take(Attr.RHO);
        final Phi name = rho.take("name");
        final Phi[] args = this.collectArgs();
        return new DispatchedUnixSyscall(new Dataized(name).asString()).call(args);
    }

    /**
     * Collects arguments for syscall from tuple.
     *
     * @return Array of arguments.
     */
    private Phi[] collectArgs() {
        final Phi args = this.take(Attr.RHO).take("args");
        final Phi retriever = args.take("at");
        final int length = new Dataized(args.take("length")).asNumber().intValue();
        final Phi[] arguments = new Phi[length];
        for (long iter = 0; iter < length; ++iter) {
            final Phi taken = retriever.copy();
            taken.put(0, new Data.ToPhi(iter));
            arguments[(int) iter] = taken;
        }
        return arguments;
    }
}
