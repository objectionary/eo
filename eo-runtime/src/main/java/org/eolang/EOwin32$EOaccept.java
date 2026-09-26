/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import org.eolang.sys.Sockaddr;
import org.eolang.sys.win32.Winsock;

/**
 * Takes the first caller waiting on a socket, as Winsock `accept` does.
 *
 * <p>Winsock hands back a handle where the POSIX call hands back a small
 * number, so what EO carries is the handle read as a number.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.accept")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOaccept extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOaccept() {
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
            Pointer.nativeValue(
                Winsock.INSTANCE.accept(
                    new Pointer(new Handle("the socket of accept", this.take("descriptor")).it()),
                    new Sockaddr(this.take("sockaddr")).it(),
                    new IntByReference(new Int(Expect.at(this, "length")).it())
                )
            )
        );
    }
}
