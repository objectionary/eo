/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Pointer;
import org.eolang.sys.Portion;
import org.eolang.sys.win32.Winsock;

/**
 * Hands bytes to whoever is on the other end of a socket, as Winsock `send`
 * does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.send")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOsend extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOsend() {
        super(
            new Attrs(
                new Attr("descriptor", new AtVoid("descriptor")),
                new Attr("buffer", new AtVoid("buffer")),
                new Attr("size", new AtVoid("size")),
                new Attr("flags", new AtVoid("flags"))
            )
        );
    }

    @Override
    public Phi lambda() {
        final byte[] chunk = new Portion(this).it();
        return new Data.ToPhi(
            Winsock.INSTANCE.send(
                new Pointer(new Dataized(this.take("descriptor")).asNumber().longValue()),
                chunk,
                chunk.length,
                new Int(Expect.at(this, "flags")).it()
            )
        );
    }
}
