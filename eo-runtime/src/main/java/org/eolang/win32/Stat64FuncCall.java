/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.WString;
import org.eolang.Cstring;
import org.eolang.Data;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The msvcrt _stat64 function call.
 *
 * <p>Fills a {@code struct _stat64} and hands its mode bits and byte size to
 * EO. That struct carries a 64-bit {@code st_size} and 64-bit
 * {@code __time64_t} timestamps, so it reports sizes past two gigabytes.</p>
 *
 * @since 0.57.0
 */
public final class Stat64FuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     *
     * @param win Win32 object
     */
    public Stat64FuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final String path = new Cstring("the 'path' argument of stat64", params[0]).it();
        final Phi result = this.win.take("return").copy();
        final WinStat info = new WinStat();
        result.put(
            0,
            new Data.ToPhi(Msvcrt.INSTANCE._wstat64(new WString(path), info))
        );
        final Phi struct = this.win.take("stat64");
        struct.put(0, new Data.ToPhi((long) (info.mode & 0xFFFF)));
        struct.put(1, new Data.ToPhi(info.bytes));
        result.put(1, struct);
        return result;
    }
}
