/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;

/**
 * Turns a socket into one that waits for callers, as `listen(2)` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.listen")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOlisten extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOlisten() {
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
            CStdLib.INSTANCE.listen(
                new Int(Expect.at(this, "v🌵descriptor")).it(),
                new Int(Expect.at(this, "v🌵backlog")).it()
            )
        );
    }
}
