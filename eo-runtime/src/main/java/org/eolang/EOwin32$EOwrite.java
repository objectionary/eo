/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

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
        final byte[] buffer = new Dataized(this.take("buffer")).take();
        final int size = new Natural(Expect.at(this, "size")).it();
        if (size > buffer.length) {
            throw new ExFailure(
                "Can't write %d bytes from a buffer of only %d bytes",
                size, buffer.length
            );
        }
        return new Data.ToPhi(
            Msvcrt.INSTANCE._write(
                new Int(Expect.at(this, "descriptor")).it(), buffer, size
            )
        );
    }
}
