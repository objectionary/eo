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

import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * Posix syscalls implementation that uses {@link CStdLib}.
 * @since 0.40
 */
public final class PosixLibWithJna implements PosixLib {
    /**
     * C standard library with syscalls.
     */
    private final CStdLib lib;

    /**
     * Ctor.
     *
     * @param lib C standard library with syscalls.
     */
    public PosixLibWithJna(final CStdLib lib) {
        this.lib = lib;
    }

    /**
     * Ctor.
     */
    public PosixLibWithJna() {
        this(CStdLib.INSTANCE);
    }

    @Override
    public Phi getpid() {
        final Phi res = new EOposix$EOres();
        res.put("code", new Data.ToPhi(this.lib.getpid()));
        res.put("output", new PhDefault());
        return res;
    }

    @Override
    public Phi write(final Long descriptor, final String buf, final Long size) {
        final Phi res = new EOposix$EOres();
        res.put("code", new Data.ToPhi(this.lib.write(descriptor, buf, size)));
        res.put("output", new PhDefault());
        return res;
    }

    @Override
    public Phi read(final Long descriptor, final byte[] buf, final Long size) {
        final Phi res = new EOposix$EOres();
        res.put("code", new Data.ToPhi(this.lib.read(descriptor, buf, size)));
        res.put("output", new PhDefault());
        return res;
    }
}
