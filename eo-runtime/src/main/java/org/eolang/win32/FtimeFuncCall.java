/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import org.eolang.Data;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The msvcrt _ftime64_s function call.
 *
 * @since 0.74.0
 */
public final class FtimeFuncCall implements Syscall {

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     *
     * @param win Win32 object
     */
    public FtimeFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        final Timeb timeb = new Timeb();
        result.put(0, new Data.ToPhi(Msvcrt.INSTANCE._ftime64_s(timeb)));
        final Phi struct = this.win.take("timeb");
        struct.put(0, new Data.ToPhi(timeb.time));
        struct.put(1, new Data.ToPhi(timeb.millitm));
        result.put(1, struct);
        return result;
    }
}
