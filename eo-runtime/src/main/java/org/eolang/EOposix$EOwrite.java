/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;

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
                new Attr("descriptor", new AtVoid("descriptor")),
                new Attr("buffer", new AtVoid("buffer")),
                new Attr("size", new AtVoid("size"))
            )
        );
    }

    @Override
    public Phi lambda() {
        final byte[] buffer = new Dataized(this.take("buffer")).take();
        final int size = new Natural(Expect.at(this, "size")).it();
        if (size > buffer.length) {
            throw new ExFailure(
                "Can't write %d bytes from a buffer of only %d bytes",
                size, buffer.length
            );
        }
        return new Data.ToPhi(
            CStdLib.INSTANCE.write(
                new Int(Expect.at(this, "descriptor")).it(), buffer, size
            )
        );
    }
}
