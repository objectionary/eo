/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.sys.win32.Msvcrt;

/**
 * Releases a descriptor, as msvcrt `_close` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.close")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOclose extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOclose() {
        super(
            new Attrs(
                new Attr(Phi.RHO, new AtRho()),
                new Attr("descriptor", new AtVoid("descriptor"))
            )
        );
    }

    @Override
    public Phi lambda() {
        final int descriptor = new Int(Expect.at(this, "descriptor")).it();
        return new Data.ToPhi(Msvcrt.INSTANCE._close(descriptor));
    }
}
