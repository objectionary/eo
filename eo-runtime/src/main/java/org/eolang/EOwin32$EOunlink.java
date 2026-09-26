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
 * Deletes a file, as msvcrt `_wunlink` does.
 *
 * <p>The code is {@code 0} when the call worked and {@code -1} when it did
 * not, and then the message says why, read out of {@code errno} before any
 * other native call can overwrite it.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.unlink")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOunlink extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOunlink() {
        super(new Attrs(new Attr("path", new AtVoid("path"))));
    }

    @Override
    public Phi lambda() {
        final String path = new Cstring(Expect.at(this, "path")).it();
        final Phi result = Phi.Φ.take("win32").take("status-return").copy();
        final int code = Msvcrt.INSTANCE._wunlink(new WString(path));
        result.put(0, new Data.ToPhi(code));
        result.put(1, new Errno(code).get());
        return result;
    }
}
