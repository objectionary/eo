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
                new Attr("descriptor", new AtVoid("descriptor")),
                new Attr("backlog", new AtVoid("backlog"))
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Data.ToPhi(
            CStdLib.INSTANCE.listen(
                new Int(Expect.at(this, "descriptor")).it(),
                new Int(Expect.at(this, "backlog")).it()
            )
        );
    }
}
