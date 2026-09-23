/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.sys.Portion;
import org.eolang.sys.win32.Msvcrt;

/**
 * Puts the head of a buffer into a descriptor, as msvcrt `_write` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.write")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOwrite extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOwrite() {
        super(
            new Attrs(
                new Attr("descriptor", new AtVoid("descriptor")),
                new Attr("buffer", new AtVoid("buffer")),
                new Attr("size", new AtVoid("size"))
            )
        );
    }

    @Override
    public Phi lambda() {
        final byte[] chunk = new Portion(this).it();
        return new Data.ToPhi(
            Msvcrt.INSTANCE._write(
                new Int(Expect.at(this, "descriptor")).it(), chunk, chunk.length
            )
        );
    }
}
