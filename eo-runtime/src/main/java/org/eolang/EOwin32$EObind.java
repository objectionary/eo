/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Pointer;
import org.eolang.sys.Sockaddr;
import org.eolang.sys.win32.Winsock;

/**
 * Gives a socket the address it will answer on, as Winsock `bind` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.bind")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EObind extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EObind() {
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
            Winsock.INSTANCE.bind(
                new Pointer(new Handle("the socket of bind", this.take("descriptor")).it()),
                new Sockaddr(this.take("sockaddr")).it(),
                new Int(Expect.at(this, "length")).it()
            )
        );
    }
}
