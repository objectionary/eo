/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Pointer;
import org.eolang.sys.Intake;
import org.eolang.sys.win32.Winsock;

/**
 * Receives up to a given number of bytes from a socket, as Winsock `recv`
 * does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.recv")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOrecv extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOrecv() {
        super(
            new Attrs(
                new Attr("v🌵descriptor", new AtVoid("v🌵descriptor")),
                new Attr("v🌵size", new AtVoid("v🌵size")),
                new Attr("v🌵flags", new AtVoid("v🌵flags"))
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Intake(
            this,
            Phi.Φ.take("win32").take("read-return"),
            (buffer, size) -> Winsock.INSTANCE.recv(
                new Pointer(new Dataized(this.take("v🌵descriptor")).asNumber().longValue()),
                buffer,
                size,
                new Int(Expect.at(this, "v🌵flags")).it()
            )
        ).it();
    }
}
