/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Pointer;
import org.eolang.sys.win32.Winsock;

/**
 * Gives a socket back, as Winsock `closesocket` does.
 *
 * <p>Winsock keeps sockets apart from the file descriptors the C runtime
 * hands out, so a socket is given back here and not through `close`.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.closesocket")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOclosesocket extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOclosesocket() {
        super(new Attrs(new Attr("descriptor", new AtVoid("descriptor"))));
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Winsock.INSTANCE.closesocket(
                new Pointer(new Dataized(this.take("descriptor")).asNumber().longValue())
            )
        );
    }
}
