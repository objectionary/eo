/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;

/**
 * Reports the identifier of the running process, as `getpid(2)` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.getpid")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOgetpid extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOgetpid() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho())));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(CStdLib.INSTANCE.getpid());
    }
}
