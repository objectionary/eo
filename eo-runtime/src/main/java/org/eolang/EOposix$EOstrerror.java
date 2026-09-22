/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.Strerror;

/**
 * Turns an error code into the message the operating system has for it, as
 * `strerror(3)` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.strerror")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOstrerror extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOstrerror() {
        super(
            new Attrs(
                new Attr(Phi.RHO, new AtRho()),
                new Attr("errno", new AtVoid("errno"))
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            new Strerror(new Int(Expect.at(this, "errno")).it()).it()
        );
    }
}
