/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.Pointer;
import org.eolang.sys.win32.Winsock;

/**
 * Turns a socket into one that waits for callers, as Winsock `listen` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.listen")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOlisten extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOlisten() {
        super(
            new Attrs(
                new Attr("v🌵descriptor", new AtVoid("v🌵descriptor")),
                new Attr("v🌵backlog", new AtVoid("v🌵backlog"))
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Winsock.INSTANCE.listen(
                new Pointer(new Dataized(this.take("v🌵descriptor")).asNumber().longValue()),
                new Int(Expect.at(this, "v🌵backlog")).it()
            )
        );
    }
}
