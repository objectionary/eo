/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.sys.Intake;

/**
 * Receives up to a given number of bytes from a socket, as `recv(2)` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.recv")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOrecv extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOrecv() {
        super(
            new Attrs(
                new Attr("descriptor"),
                new Attr("size"),
                new Attr("flags")
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Intake(
            this.expected("size"),
            Phi.Φ.take("posix").take("read-return"),
            (buffer, size) -> CStdLib.INSTANCE.recv(
                new Int(this.expected("descriptor")).it(),
                buffer,
                size,
                new Int(this.expected("flags")).it()
            )
        ).it();
    }
}
