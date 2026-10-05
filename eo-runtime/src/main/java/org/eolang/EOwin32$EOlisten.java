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
                new Attr("descriptor", new AtVoid("descriptor")),
                new Attr("backlog", new AtVoid("backlog"))
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            Winsock.INSTANCE.listen(
                new Pointer(new Dataized(this.take("descriptor")).asNumber().longValue()),
                new Int(Expect.at(this, "backlog")).it()
            )
        );
    }
}
