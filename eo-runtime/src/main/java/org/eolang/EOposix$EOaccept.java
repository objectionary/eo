/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.ptr.IntByReference;
import org.eolang.posix.CStdLib;
import org.eolang.sys.Sockaddr;

/**
 * Takes the first caller waiting on a socket, as POSIX `accept` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.accept")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOaccept extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOaccept() {
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
            CStdLib.INSTANCE.accept(
                new Int(Expect.at(this, "descriptor")).it(),
                new Sockaddr(this.take("sockaddr")).it(),
                new IntByReference(new Int(Expect.at(this, "length")).it())
            )
        );
    }
}
