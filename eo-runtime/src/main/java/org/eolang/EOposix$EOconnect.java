/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.sys.Sockaddr;

/**
 * Reaches the address a socket is pointed at, as `connect(2)` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.connect")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOconnect extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOconnect() {
        super(
            new Attrs(
                new Attr("descriptor", new AtVoid("descriptor")),
                new Attr("sockaddr", new AtVoid("sockaddr")),
                new Attr("length", new AtVoid("length"))
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            CStdLib.INSTANCE.connect(
                new Int(Expect.at(this, "descriptor")).it(),
                new Sockaddr(this.take("sockaddr")).it(),
                new Int(Expect.at(this, "length")).it()
            )
        );
    }
}
