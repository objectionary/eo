/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.sys.Portion;

/**
 * Hands bytes to whoever is on the other end of a socket, as POSIX `send`
 * does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.send")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOsend extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOsend() {
        super(
            new Attrs(
                new Attr("v🌵descriptor"),
                new Attr("v🌵buffer"),
                new Attr("v🌵size"),
                new Attr("v🌵flags")
            )
        );
    }

    @Override
    public Phi lambda() {
        final byte[] chunk = new Portion(this).it();
        return new Data.ToPhi(
            CStdLib.INSTANCE.send(
                new Int(Expect.at(this, "v🌵descriptor")).it(),
                chunk,
                chunk.length,
                new Int(Expect.at(this, "v🌵flags")).it()
            )
        );
    }
}
