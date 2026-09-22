/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.WString;
import org.eolang.sys.Cstring;
import org.eolang.sys.win32.Msvcrt;

/**
 * Tells whether a path is reachable under a given mode, as msvcrt `_waccess` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.access")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOaccess extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOaccess() {
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
        return new Data.ToPhi(
            Msvcrt.INSTANCE._waccess(
                new WString(new Cstring(Expect.at(this, "path")).it()),
                new Int(Expect.at(this, "mode")).it()
            )
        );
    }
}
