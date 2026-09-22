/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.sys.Cstring;

/**
 * Tells whether a path is reachable under a given mode, as `access(2)` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.access")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOaccess extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOaccess() {
        super(
            new Attrs(
                new Attr(Phi.RHO, new AtRho()),
                new Attr("path", new AtVoid("path")),
                new Attr("mode", new AtVoid("mode"))
            )
        );
    }

    @Override
    public Phi lambda() {
        final int mode = new Int(Expect.at(this, "mode")).it();
        final String path = new Cstring(Expect.at(this, "path")).it();
        return new Data.ToPhi(CStdLib.INSTANCE.access(path, mode));
    }
}
