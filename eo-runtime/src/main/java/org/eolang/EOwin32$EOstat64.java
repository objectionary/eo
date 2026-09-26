/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.sun.jna.WString;
import org.eolang.sys.Cstring;
import org.eolang.sys.win32.Msvcrt;
import org.eolang.sys.win32.WinStat;

/**
 * Tells the mode bits and the size of the file a path leads to, as msvcrt
 * `_wstat64` does.
 *
 * <p>The struct it fills carries a 64-bit size and 64-bit timestamps, so it
 * reports sizes past two gigabytes.</p>
 *
 * @since 0.77.0
 * @checkstyle IllegalIdentifierNameCheck (6 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "win32.stat64")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOstat64 extends PhDefault implements Atom {

    /**
     * Ctor.
     */
    public EOwin32$EOstat64() {
        super(new Attrs(new Attr("path", new AtVoid("path"))));
    }

    @Override
    public Phi lambda() {
        final WString path = new WString(new Cstring(Expect.at(this, "path")).it());
        final WinStat info = new WinStat();
        final Phi result = Phi.Φ.take("win32").take("stat-return").copy();
        result.put(0, new Data.ToPhi(Msvcrt.INSTANCE._wstat64(path, info)));
        result.put(1, new Data.ToPhi((long) (info.mode & 0xFFFF)));
        result.put(2, new Data.ToPhi(info.bytes));
        return result;
    }
}
