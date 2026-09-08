/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.win32.NamedFuncCall;

/**
 * Win32 function call.
 *
 * @since 0.40
 * @checkstyle IllegalIdentifierNameCheck (20 lines)
 * @checkstyle TypeNameCheck (19 lines)
 */
@XmirObject(oname = "win32.@")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOφ extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOφ() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho())));
    }

    @Override
    public Phi lambda() {
        final Phi rho = this.take(Phi.RHO);
        return new NamedFuncCall(
            new Dataized(rho.take("name")).asString(),
            rho
        ).make(new TupleToArray(rho.take("args")).get());
    }
}
