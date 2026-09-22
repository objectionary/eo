/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;

/**
 * Releases a descriptor, as `close(2)` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.close")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOclose extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOclose() {
        super(
            new Attrs(
                new Attr(Phi.RHO, new AtRho()),
                new Attr("descriptor", new AtVoid("descriptor"))
            )
        );
    }

    @Override
    public Phi lambda() {
        final int descriptor = new Int(Expect.at(this, "descriptor")).it();
        return new Data.ToPhi(CStdLib.INSTANCE.close(descriptor));
    }
}
