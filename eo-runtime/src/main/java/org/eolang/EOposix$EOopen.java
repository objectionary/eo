/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.posix.Errno;
import org.eolang.sys.Cstring;

/**
 * Opens a file with the given flags, creating it with the given permission
 * bits when the flags ask for that, as POSIX `open` does.
 *
 * <p>The code is {@code -1} when the call did not work, and then the message
 * says why, read out of {@code errno} before any other native call can
 * overwrite it.</p>
 *
 * <p>The code is the descriptor of the opened file when the call worked.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.open")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOopen extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOopen() {
        super(
            new Attrs(
                new Attr("path", new AtVoid("path")),
                new Attr("flags", new AtVoid("flags")),
                new Attr("mode", new AtVoid("mode"))
            )
        );
    }

    @Override
    public Phi lambda() {
        final String path = new Cstring(Expect.at(this, "path")).it();
        final int flags = new Int(Expect.at(this, "flags")).it();
        final int mode = new Int(Expect.at(this, "mode")).it();
        final Phi result = Phi.Φ.take("posix").take("status-return").copy();
        final int code = CStdLib.INSTANCE.open(path, flags, mode);

        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
