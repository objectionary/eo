/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.sys.win32.Msvcrt;

/**
 * Reports the identifier of the running process, as msvcrt `_getpid` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.getpid")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOgetpid extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOgetpid() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho())));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(Msvcrt.INSTANCE._getpid());
    }
}
