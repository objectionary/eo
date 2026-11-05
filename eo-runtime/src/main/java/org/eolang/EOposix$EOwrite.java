/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.sys.Portion;

/**
 * Puts the head of a buffer into a descriptor, as `write(2)` does.
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.write")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOwrite extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOwrite() {
        super(
            new Attrs(
                new Attr("descriptor"),
                new Attr("buffer"),
                new Attr("size")
            )
        );
    }

    @Override
    public Phi lambda() {
        final byte[] chunk = new Portion(this.free("buffer"), this.expected("size")).it();
        return new Data.ToPhi(
            CStdLib.INSTANCE.write(
                new Int(this.expected("descriptor")).it(), chunk, chunk.length
            )
        );
    }
}
