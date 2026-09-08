/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.NamedSyscall;

/**
 * Posix syscall.
 *
 * @since 0.40
 * @checkstyle IllegalIdentifierNameCheck (20 lines)
 * @checkstyle TypeNameCheck (19 lines)
 */
@XmirObject(oname = "posix.@")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOφ extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOφ() {
        super(new Attrs(new Attr(Phi.RHO, new AtRho())));
    }

    @Override
    public Phi lambda() {
        final Phi rho = this.take(Phi.RHO);
        return new NamedSyscall(
            new Dataized(rho.take("name")).asString(),
            rho
        ).make(new TupleToArray(rho.take("args")).get());
    }
}
