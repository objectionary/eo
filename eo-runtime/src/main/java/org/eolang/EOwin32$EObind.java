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
                new Attr("v🌵descriptor", new AtVoid("v🌵descriptor")),
                new Attr("v🌵sockaddr", new AtVoid("v🌵sockaddr")),
                new Attr("v🌵length", new AtVoid("v🌵length"))
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Winsock.INSTANCE.bind(
                new Pointer(new Dataized(this.take("v🌵descriptor")).asNumber().longValue()),
                new Sockaddr(this.take("v🌵sockaddr")).it(),
                new Int(Expect.at(this, "v🌵length")).it()
            )
        );
    }
}
