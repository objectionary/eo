/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.sys.Intake;

/**
 * Reads up to a given number of bytes from a descriptor, as `read(2)` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.read")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOread extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOread() {
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
            Phi.Φ.take("posix").take("read-return"),
            (buffer, size) -> CStdLib.INSTANCE.read(
                new Int(Expect.at(this, "descriptor")).it(), buffer, size
            )
        ).it();
    }
}
