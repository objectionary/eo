/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.sys.Sockaddr;

/**
 * Gives a socket the address it will answer on, as `bind(2)` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.bind")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EObind extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EObind() {
        super(
            new Attrs(
                new Attr("v🌵descriptor"),
                new Attr("v🌵sockaddr"),
                new Attr("v🌵length")
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            CStdLib.INSTANCE.bind(
                new Int(Expect.at(this, "v🌵descriptor")).it(),
                new Sockaddr(this.take("v🌵sockaddr")).it(),
                new Int(Expect.at(this, "v🌵length")).it()
            )
        );
    }
}
