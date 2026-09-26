/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.sys.Intake;
import org.eolang.sys.win32.Msvcrt;

/**
 * Reads up to a given number of bytes from a descriptor, as msvcrt `_read`
 * does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.read")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOread extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOread() {
        super(
            new Attrs(
                new Attr("descriptor", new AtVoid("descriptor")),
                new Attr("size", new AtVoid("size"))
            )
        );
    }

    @Override
    public Phi lambda() {
        return new Intake(
            this,
            Phi.Φ.take("win32").take("read-return"),
            (buffer, size) -> Msvcrt.INSTANCE._read(
                new Int(Expect.at(this, "descriptor")).it(), buffer, size
            )
        ).it();
    }
}
