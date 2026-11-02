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
                new Attr("descriptor", new AtVoid("descriptor")),
                new Attr("size", new AtVoid("size")),
                new Attr("flags", new AtVoid("flags"))
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Intake(
            this,
            Phi.Φ.take("win32").take("read-return"),
            (buffer, size) -> Winsock.INSTANCE.recv(
                new Pointer(new Handle("the socket of recv", this.take("descriptor")).it()),
                buffer,
                size,
                new Int(Expect.at(this, "flags")).it()
            )
        ).it();
    }
}
