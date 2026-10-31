/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;

/**
 * Makes an endpoint for communication, as `socket(2)` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.socket")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOsocket extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOsocket() {
        super(
            new Attrs(
                new Attr("domain", new AtVoid("domain")),
                new Attr("type", new AtVoid("type")),
                new Attr("protocol", new AtVoid("protocol"))
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            CStdLib.INSTANCE.socket(
                new Int(Expect.at(this, "domain")).it(),
                new Int(Expect.at(this, "type")).it(),
                new Int(Expect.at(this, "protocol")).it()
            )
        );
    }
}
