/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.WString;
import org.eolang.sys.Cstring;
import org.eolang.sys.win32.Errno;
import org.eolang.sys.win32.Msvcrt;

/**
 * Moves a file or a directory to another path, as msvcrt `_wrename` does.
 *
 * <p>The code is {@code -1} when the call did not work, and then the message
 * says why, read out of {@code errno} before any other native call can
 * overwrite it.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.rename")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOrename extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOrename() {
        super(
            new Attrs(
                new Attr("from", new AtVoid("from")),
                new Attr("to", new AtVoid("to"))
            )
        );
    }

    @Override
    public Phi lambda() {
        final String from = new Cstring(Expect.at(this, "from")).it();
        final String target = new Cstring(Expect.at(this, "to")).it();
        final Phi result = Phi.Φ.take("win32").take("status-return").copy();
        final int code = Msvcrt.INSTANCE._wrename(new WString(from), new WString(target));
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
