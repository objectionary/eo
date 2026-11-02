/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.eolang.posix.CStdLib;
import org.eolang.posix.Errno;
import org.eolang.sys.Cstring;

/**
 * Moves a file or a directory to another path, as POSIX `rename` does.
 *
 * <p>The code is {@code -1} when the call did not work, and then the message
 * says why, read out of {@code errno} before any other native call can
 * overwrite it.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "posix.rename")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOrename extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOposix$EOrename() {
        super(
            new Attrs(
                new Attr("v🌵from", new AtVoid("v🌵from")),
                new Attr("v🌵to", new AtVoid("v🌵to"))
            )
        );
    }

    @Override
    public Phi lambda() {
        final String from = new Cstring(Expect.at(this, "v🌵from")).it();
        final String target = new Cstring(Expect.at(this, "v🌵to")).it();
        final Phi result = Phi.Φ.take("posix").take("status-return").copy();
        final int code = CStdLib.INSTANCE.rename(from, target);
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
