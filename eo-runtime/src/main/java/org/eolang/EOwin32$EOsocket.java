/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Pointer;
import org.eolang.sys.win32.Winsock;

/**
 * Makes an endpoint for communication, as Winsock `socket` does.
 *
 * <p>Winsock hands back a handle where the POSIX call hands back a small
 * number, so what EO carries is the handle read as a number.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.socket")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOsocket extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOsocket() {
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
            Pointer.nativeValue(
                Winsock.INSTANCE.socket(
                    new Int(Expect.at(this, "domain")).it(),
                    new Int(Expect.at(this, "type")).it(),
                    new Int(Expect.at(this, "protocol")).it()
                )
            )
        );
    }
}
