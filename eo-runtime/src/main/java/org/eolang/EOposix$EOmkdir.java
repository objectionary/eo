/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.posix.Errno;
import org.eolang.sys.Cstring;

/**
 * Makes a directory with the given permission bits, as POSIX `mkdir` does.
 *
 * <p>The code is {@code 0} when the call worked and {@code -1} when it did
 * not, and then the message says why, read out of {@code errno} before any
 * other native call can overwrite it.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.mkdir")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOmkdir extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOmkdir() {
        super(
            new Attrs(
                new Attr("path", new AtVoid("path")),
                new Attr("mode", new AtVoid("mode"))
            )
        );
    }

    @Override
    public Phi lambda() {
        final String path = new Cstring(Expect.at(this, "path")).it();
        final int mode = new Int(Expect.at(this, "mode")).it();
        final Phi result = Phi.Φ.take("posix").take("status-return").copy();
        final int code = CStdLib.INSTANCE.mkdir(path, mode);
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
